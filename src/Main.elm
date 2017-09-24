port module Main exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onWithOptions)
import Html.Keyed as Keyed exposing (..)
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as P exposing (decode, optional, required)
import Markdown exposing (toHtml)
import Navigation exposing (Location)
import Result exposing (..)
import Route exposing (..)
import Svg
import Svg.Attributes as SA


-- PORTS


port requestFeed : String -> Cmd msg


port requestItem : Int -> Cmd msg


port requestUser : String -> Cmd msg


port feedSubscription : ({ feed : String, data : List Int } -> msg) -> Sub msg


port itemSubscription : (Value -> msg) -> Sub msg


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { route : Route
    , feeds : Dict.Dict String (List Int)
    , items : Dict.Dict Int (RemoteData Item)
    }



--INIT


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    toRequest
        { route = parseLocation location
        , feeds = Dict.empty
        , items = Dict.empty
        }



-- UPDATE


type Msg
    = NewUrl String
    | UrlChange Location
    | GotFeed { feed : String, data : List Int }
    | GotItem (RemoteData Item)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            model ! [ Navigation.newUrl url ]

        UrlChange location ->
            toRequest { model | route = parseLocation location }

        GotFeed { feed, data } ->
            let
                firstPage =
                    List.take 30 data

                subscribeTo =
                    List.filter (not << flip Dict.member model.items) firstPage

                newItems =
                    List.foldl (\x -> Dict.insert x (Requested x)) model.items subscribeTo
            in
            { model
                | feeds = Dict.insert feed data model.feeds
                , items = newItems
            }
                ! itemsAdded subscribeTo

        GotItem item ->
            { model | items = Dict.insert (remoteDataItemId item) item model.items } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ feedSubscription GotFeed
        , itemSubscription decodeItem
        ]


remoteDataItemId remoteData =
    case remoteData of
        Subscribed x ->
            .id x

        _ ->
            0


itemsAdded items =
    List.map requestItem items



-- VIEW


view : Model -> Html Msg
view { route, feeds, items } =
    let
        routeView =
            case route of
                NotFound ->
                    notFoundView

                ItemRoute id ->
                    case fromItems id items of
                        Subscribed x ->
                            itemView x

                        _ ->
                            loadingView

                -- User name ->
                --     handleViewState userView name
                _ ->
                    listView (listFromItems route feeds items)
    in
    main_ []
        [ headerView route
        , routeView
        ]


feedAtRoute feed route =
    Dict.get (Route.toApi route) feed
        |> Maybe.withDefault []


listFromItems route feeds items =
    feedAtRoute feeds route
        |> List.filterMap (\x -> Dict.get x items)


fromItems id items =
    Dict.get id items
        |> Maybe.withDefault NotAsked



-- HEADER VIEW


headerView : Route -> Html Msg
headerView route =
    header []
        [ logo
        , nav [] (List.map (headerLink route) [ Top, New, Ask, Show, Jobs ])
        ]


headerLink : Route -> Route -> Html Msg
headerLink currentRoute route =
    if currentRoute == route then
        span [] [ text (Route.toTitle route) ]
    else
        link route [ text (Route.toTitle route) ]



-- LIST VIEW


listView : List (RemoteData Item) -> Html Msg
listView feed =
    Keyed.ul [ class "list-view" ] (List.indexedMap listViewItem feed)


listViewItem : Int -> RemoteData Item -> ( String, Html Msg )
listViewItem index item =
    case item of
        Subscribed item ->
            ( toString item.id
            , li []
                [ aside [] [ text (toString item.points) ]
                , div []
                    [ itemUrl item.id item.url item.title
                    , span [ class "domain" ] [ text "" ]
                    , itemFooter item
                    ]
                ]
            )

        Requested id ->
            ( toString id, li [] [ text "requested" ] )

        _ ->
            ( "", text "" )


itemUrl : Int -> String -> String -> Html Msg
itemUrl id url title =
    if String.contains "item?id=" url then
        link (Route.ItemRoute id) [ text title ]
    else
        a [ href url, target "_blank" ] [ text title ]



-- ITEM VIEW


itemView : Item -> Html Msg
itemView item =
    article []
        [ section []
            [ h2 [] [ text item.title ]
            , span [ class "domain" ] [ text "" ]
            , itemFooter item
            ]
        , Markdown.toHtml [] item.content

        -- , section [ class "comments-view" ]
        --     [ commentsView (getComments item.comments)
        --     ]
        ]


itemFooter : Item -> Html Msg
itemFooter item =
    if item.type_ == "job" then
        footer [] [ text "" ]
    else
        footer []
            [ text (toString item.points ++ " points by ")
            , link (Route.User item.user) [ text item.user ]

            -- , text (" " ++ item.timeAgo ++ " | ")
            , link (Route.ItemRoute item.id) [ text " link" ]
            ]



-- COMMENTS VIEW
-- commentsView : List Item -> Html Msg
-- commentsView comments =
--     ul [] (List.map commentView comments)
-- commentView : Item -> Html Msg
-- commentView item =
--     li []
--         [ div [ class "comment-meta" ]
--             [ link (Route.User item.user) [ text item.user ]
--             , text (" " ++ item.timeAgo)
--             ]
--         , Markdown.toHtml [] item.content
--         , commentsView (getComments item.comments)
--         ]
-- USER VIEW


userView : User -> Html Msg
userView user =
    section [ class "user-view" ]
        [ table []
            [ row "user:" user.id
            , row "created:" user.created
            , row "karma:" (toString user.karma)
            , row "about:" user.about
            ]
        ]


row : String -> String -> Html Msg
row x y =
    tr []
        [ td [] [ text x ]
        , td [] [ text y ]
        ]



-- VIEW HELPERS


handleViewState : (a -> Html Msg) -> RemoteData a -> Html Msg
handleViewState successView remoteData =
    case remoteData of
        NotAsked ->
            loadingView

        Requested _ ->
            loadingView

        Failure ->
            errorView

        Subscribed x ->
            successView x


loadingView : Html Msg
loadingView =
    div [ class "notification" ] [ div [ class "spinner" ] [] ]


notFoundView : Html Msg
notFoundView =
    div [ class "notification" ] [ text "404" ]


errorView : Html Msg
errorView =
    div [ class "notification" ] [ text "error" ]


link : Route -> List (Html Msg) -> Html Msg
link route kids =
    a [ href (Route.toUrl route), onClickPreventDefault (Route.toUrl route) ] kids


onClickPreventDefault : String -> Attribute Msg
onClickPreventDefault url =
    onWithOptions "click"
        { preventDefault = True
        , stopPropagation = False
        }
        (D.succeed <| NewUrl url)


getComments : Comments -> List Item
getComments x =
    case x of
        Comments xs ->
            xs



-- ROUTE TO REQUEST


toRequest : Model -> ( Model, Cmd Msg )
toRequest ({ route, feeds, items } as model) =
    case route of
        NotFound ->
            ( model, Cmd.none )

        ItemRoute x ->
            if Dict.member x items then
                ( model, Cmd.none )
            else
                ( model, requestItem x )

        _ ->
            if Dict.member (Route.toApi route) feeds then
                ( model, Cmd.none )
            else
                ( model, requestFeed (Route.toApi route) )



--DECODERS


decodeItem item =
    case decodeValue itemDecoder item of
        Ok item ->
            GotItem (Subscribed item)

        Err _ ->
            GotItem Failure


itemDecoder : D.Decoder Item
itemDecoder =
    P.decode Item
        |> P.required "id" D.int
        |> P.optional "title" D.string "No title"
        |> P.optional "score" D.int 0
        |> P.optional "by" D.string "No user found"
        |> P.optional "url" D.string ""
        |> P.optional "text" D.string ""
        |> P.optional "type" D.string ""


decodeUser : D.Decoder User
decodeUser =
    P.decode User
        |> P.optional "title" D.string ""
        |> P.required "created" D.string
        |> P.required "id" D.string
        |> P.required "karma" D.int


decodeComments : Decoder Comments
decodeComments =
    D.map Comments (D.list (D.lazy (\_ -> itemDecoder)))



-- TYPES


type RemoteData a
    = NotAsked
    | Requested Int
    | Failure
    | Subscribed a


type alias Item =
    { id : Int
    , title : String
    , points : Int
    , user : String
    , url : String
    , content : String
    , type_ : String
    }


type alias User =
    { about : String
    , created : String
    , id : String
    , karma : Int
    }


type Comments
    = Comments (List Item)



-- LOGO


logo : Svg.Svg Msg
logo =
    Svg.svg [ width 25, height 26, SA.viewBox "0 0 25 26" ]
        [ Svg.g [ SA.fill "none" ]
            [ Svg.path [ SA.fill "#F0AD00", SA.d "M12.4 6l5.3.2L12.3.8m0 12.5v5.3l5.4-5.3" ] []
            , Svg.path [ SA.fill "#7FD13B", SA.d "M12.3 25v-5.3l6-6v5.5m-6-12.4h6v5.8h-6z" ] []
            , Svg.path [ SA.fill "#60B5CC", SA.d "M19 18.4l5.3-5.4L19 7.5" ] []
            , Svg.path [ SA.fill "#5A6378", SA.d "M11.7.8H0l11.7 11.7" ] []
            , Svg.path [ SA.fill "#60B5CC", SA.d "M11.7 25.2V13.5L0 25.2" ] []
            ]
        ]
