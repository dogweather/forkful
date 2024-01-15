---
title:                "Versenden einer HTTP-Anfrage mit einfacher Authentifizierung"
html_title:           "Javascript: Versenden einer HTTP-Anfrage mit einfacher Authentifizierung"
simple_title:         "Versenden einer HTTP-Anfrage mit einfacher Authentifizierung"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Warum

Das Senden von HTTP-Anfragen mit Basic-Authentifizierung kann für Entwickler*innen von Vorteil sein, um Zugriff auf geschützte Ressourcen auf Webservern zu erhalten. Dies kann beispielsweise bei der Integration von externen APIs oder beim Aufbau von Benutzerauthentifizierungssystemen nützlich sein.

# Anleitung

Um eine HTTP-Anfrage mit Basic-Authentifizierung in Javascript zu senden, können wir die `fetch()`-Methode verwenden. Dabei muss der URL-Parameter der angeforderten Ressource sowie die`authentication` Option übergeben werden, die einen Objekt mit`username` und `password` Eigenschaften enthält. Hier ist ein Beispielcode, der eine GET-Anfrage an eine geschützte Ressource sendet und die Antwort in der Konsole ausgibt:

```Javascript
fetch('https://www.meinegeschuetzteapi.com/ressource', {
  method: 'GET',
  authentication: {
    username: 'Benutzername',
    password: 'Passwort'
  }
})
  .then(response => response.text())
  .then(data => console.log(data))
  .catch(error => console.error(error));
```

Das oben genannte Beispiel verwendet die moderne `fetch()`-Methode und verwendet Promises für die asynchrone Verarbeitung von Anfragen.

# Tiefere Einblicke

Es ist wichtig zu beachten, dass die Verwendung von Basic-Authentifizierung eine grundlegende und nicht besonders sichere Authentifizierungsmethode ist. Das Passwort wird immer im Klartext übertragen, was anfällig für Man-in-the-Middle-Angriffe macht. Aus diesem Grund wird empfohlen, die Verwendung von Basic-Authentifizierung zu vermeiden, wenn möglich, und stattdessen auf sicherere Authentifizierungsmethoden wie OAuth 2.0 zu setzen.

# Siehe auch

- [Javascript Fetch API Dokumentation] (https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [HTTP Basic Authentication in Wikipedia] (https://en.wikipedia.org/wiki/Basic_access_authentication)