import 'style.scss';
// import 'register-sw.js';
import Firebase from 'firebase/app';
import 'firebase/database';
import { Main as Elm } from 'Main';

var config = {
    databaseURL: 'https://hacker-news.firebaseio.com'
};

Firebase.initializeApp(config);

var app = Elm.fullscreen();
var api = Firebase.database().ref("v0");

window.addEventListener("load", function() {
    document.querySelector(".notification").remove();
});

app.ports.requestItem.subscribe(function(id) {
    api.child("item").child(id).on("value", function(item) {
        console.log('onItem')
        app.ports.itemSubscription.send(item.val());
    })
});

app.ports.requestFeed.subscribe(function(feed) {
    api.child(feed).once("value", function(x) {
        console.log('onFeed')
        app.ports.feedSubscription.send({feed: feed, data: x.val()});
    });
});

