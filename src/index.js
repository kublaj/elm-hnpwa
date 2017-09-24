import 'style.scss';
import 'register-sw.js';
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

app.ports.getItem.subscribe(function(id) {
    console.log(id)
    getItem(id).then(function(item) {
        // app.ports.suggestions.send(suggestions);
        console.log(item.val())
    })
});

app.ports.getPage.subscribe(function(page) {
    getPage(page).then(function(pageList) {
        pageList.forEach(function(x) {
            var id = parseInt(x);
            console.log(x.val())
            app.ports.gotId.send(x.val());
        })
    })
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
