require('style.scss');
require('register-sw.js');
import Firebase from 'firebase/app';
import 'firebase/database';

var config = {
    databaseURL: 'https://hacker-news.firebaseio.com'
};

Firebase.initializeApp(config);

var Elm = require('Main');
var app = Elm.Main.fullscreen();
var api = Firebase.database().ref("v0");

window.addEventListener("load", function() {
    document.querySelector(".notification").remove();

});

function getPage(title) {
    return api.child(title).once("value", function(snapshot) {
        return snapshot.val();
    });
}

function getItem(id) {
    return api.child(id).on("value", function(snapshot) {
        return snapshot.val();
    });
}
