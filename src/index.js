import { Main as Elm } from 'Main';
import Firebase from 'firebase/app';
import 'firebase/database';
import 'style.scss';
import 'register-sw.js';

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
        console.log(id, ' updated')
        app.ports.itemSubscription.send(item.val());
    })
});

app.ports.requestFeed.subscribe(function(feed) {
    api.child(feed).on("value", function(x) {
        console.log(feed, ' updated')
        app.ports.feedSubscription.send({feed: feed, data: x.val()});
    });
});

