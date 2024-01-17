---
title:                "Eine http-Anfrage mit grundlegender Authentifizierung senden"
html_title:           "Javascript: Eine http-Anfrage mit grundlegender Authentifizierung senden"
simple_title:         "Eine http-Anfrage mit grundlegender Authentifizierung senden"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Wenn du als Programmierer eine HTTP-Anfrage mit grundlegender Authentifizierung sendest, bedeutet das, dass du eine Anfrage an eine Webseite oder eine API schickst und dabei einen Benutzernamen und ein Passwort mitschickst. Das ist nützlich, wenn du auf eine geschützte Seite oder eine API zugreifen möchtest, die vertrauliche Informationen enthält.

## Wie geht's?

```Javascript
const request = require('request');

// Definiere die Optionen für die Anfrage mit grundlegender
// Authentifizierung
var options = {
    url: 'http://beispiel.com/api',
    auth: {
        user: 'Benutzername',
        password: 'Passwort'
    }
};

// Sende die Anfrage
request(options, (error, response, body) => {
    if (error) throw new Error(error);
    // Gib die Antwort aus
    console.log(body);
});
```

## Tiefer eintauchen

Die grundlegende Authentifizierung ist eine der ältesten Authentifizierungsmethoden im World Wide Web. Sie wurde ursprünglich für das HTTP-Protokoll entwickelt, welches später zum Grundgerüst des Internets wurde. Es gibt jedoch auch alternative Methoden, wie zum Beispiel die OAuth-Authentifizierung, die heutzutage häufiger verwendet wird. Die Implementierung der grundlegenden Authentifizierung kann je nach Programmiersprache bzw. Framework variieren, aber im Grunde funktioniert sie immer auf dieselbe Weise: Der Benutzername und das Passwort werden zusammen mit der Anfrage an den Server geschickt, der diese dann überprüft und bei erfolgreicher Authentifizierung den Zugriff gewährt.

## Sieh auch

- [Node.js Dokumentation zur grundlegenden Authentifizierung](https://nodejs.org/api/http.html#http_http_authorization)
- [Eine Einführung in REST und grundlegende Authentifizierung](https://www.robinwieruch.de/rest-basic-authentication-node-js)