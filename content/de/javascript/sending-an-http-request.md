---
title:                "Eine HTTP-Anfrage senden"
html_title:           "Javascript: Eine HTTP-Anfrage senden"
simple_title:         "Eine HTTP-Anfrage senden"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage ist einfach gesagt die Art und Weise, wie ein Programm mit einem externen Server kommuniziert, um Informationen auszutauschen. Programmierer tun das, um zum Beispiel Daten von einer Datenbank abzurufen oder um mit einer API zu integrieren.

## Wie geht's?

Um eine HTTP-Anfrage in Javascript zu senden, können wir die ```fetch``` Funktion verwenden. Hier ist ein Beispiel für eine GET-Anfrage:

```Javascript
fetch("https://api.example.com/users")
  .then(response => response.json())
  .then(data => {
    console.log(data);
  })
  .catch(error => {
    console.error(error);
  });
```

In diesem Beispiel wird eine GET-Anfrage an die URL "https://api.example.com/users" gesendet und die Antwort als JSON-Objekt ausgegeben. Wir können auch Daten in unsere Anfrage einschließen, indem wir ein Objekt als zweiten Parameter an die ```fetch``` Funktion übergeben. Hier ist ein Beispiel für eine POST-Anfrage:

```Javascript
const request = {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ username: 'John', password: 'pass123' })
}

fetch("https://api.example.com/login", request)
  .then(response => response.json())
  .then(data => {
    console.log(data);
  })
  .catch(error => {
    console.error(error);
  });
```

Dieses Mal wird ein Objekt mit den Anfrage-Parametern erstellt und als zweiter Parameter an die ```fetch``` Funktion übergeben. Dadurch wird eine JSON-kodierte Anfrage an die URL "https://api.example.com/login" gesendet.

## Tiefer tauchen

Früher war es üblich, XMLHttpRequest (XHR) zu verwenden, um HTTP-Anfragen in Javascript zu senden. Diese Methode ist jedoch veraltet und wurde durch die ```fetch``` Funktion ersetzt. Eine andere Alternative ist die Verwendung von Bibliotheken wie jQuery oder axios, die das Senden von HTTP-Anfragen vereinfachen. 

Es ist auch wichtig zu beachten, dass die ```fetch``` Funktion standardmäßig keine Cookies oder Authentifizierungsheader überträgt. Um dies zu ermöglichen, müssen wir die Optionen ```credentials``` und ```headers``` in unserer Anfrage angeben.

## Siehe auch

- [MDN Dokumentation zur fetch Funktion](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- [W3Schools Anleitung zur HTTP-Anfrage in Javascript](https://www.w3schools.com/js/js_ajax_http.asp)