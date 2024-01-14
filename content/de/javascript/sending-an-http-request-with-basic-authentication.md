---
title:                "Javascript: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Das Senden von HTTP-Anfragen mit grundlegender Authentifizierung ist essentiell für die Sicherheit und Authentifizierung von Webanwendungen. Es ermöglicht den Benutzern, ihre Daten auf Websites zu schützen und den Zugriff auf bestimmte Ressourcen zu kontrollieren.

## Wie geht das?

Um eine HTTP-Anfrage mit grundlegender Authentifizierung zu senden, müssen Sie die "fetch" API von Javascript verwenden. Hier ist ein Beispielcode:

```Javascript
fetch('URL', {
  method: 'GET', // Hier können Sie auch andere Methoden wie "POST" oder "PUT" verwenden.
  headers: {
    'Authorization': 'Basic ' + btoa('Benutzername:Passwort') // Hier wird der Benutzername und das Passwort kodiert und als Basis64-String gesendet.
  }
})
.then(response => response.text())
.then(data => console.log(data)) // Hier wird die Ausgabe der Anfrage in der Konsole ausgegeben.
```

Die Ausgabe dieser Anfrage wird der Inhalt der angeforderten Ressource sein. Wenn die Authentifizierung fehlschlägt, wird eine Fehlermeldung zurückgegeben.

## Tieferer Einblick

Die grundlegende Authentifizierung ist keine sichere Methode, da Benutzername und Passwort in Klartext über das Netzwerk gesendet werden. Es wird empfohlen, HTTPS zu verwenden, um die Kommunikation zu verschlüsseln. Außerdem können Sie die "Authorization" Header-Option verwenden, um verschiedene Arten von Authentifizierung wie OAuth oder Bearer Token zu implementieren.

## Siehe auch

- [Dokumentation zur fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Tutorial zur grundlegenden Authentifizierung in Node.js](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)