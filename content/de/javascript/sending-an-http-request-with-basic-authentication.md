---
title:                "HTTP-Anfragen mit Basisauthentifizierung senden"
date:                  2024-01-20T18:02:06.432200-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-Anfragen mit Basisauthentifizierung senden"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Requests mit Basisauthentifizierung schicken Daten sicher über das Netz. Entwickler nutzen das, um vertrauliche Daten wie Benutzernamen und Passwörter zu schützen.

## How to:
```Javascript
const axios = require('axios');

const username = 'deinUsername';
const password = 'deinPasswort';
const base64Credentials = Buffer.from(`${username}:${password}`).toString('base64');

axios.get('https://deinbeispiel.de/daten', {
  headers: {
    'Authorization': `Basic ${base64Credentials}`
  }
})
.then(response => {
  console.log('Daten erfolgreich abgerufen:', response.data);
})
.catch(error => {
  console.error('Fehler beim Abrufen der Daten:', error);
});
```

## Deep Dive
Basisauthentifizierung (Basic Authentication) ist ein alter Mechanismus aus den frühen HTTP-Tagen, um Login-Daten sicher zu übermitteln. Kombiniert mit HTTPS ist es sicherer, aber es gibt modernere Alternativen, wie OAuth.

Die Authentifizierung wird als Base64-codierter String gesendet, der im `Authorization`-Header der HTTP-Anfrage platziert wird. Die Base64-Codierung ist nicht verschlüsselt, verschleiert die Credentials nur leicht. Wichtig ist, HTTPS statt HTTP zu verwenden, damit die Credentials nicht im Klartext im Netzwerk sichtbar sind.

Dein Server oder API, der die Anfrage empfängt, muss dann diesen Header prüfen und bestätigen, dass Username und Passwort gültig sind.

## Siehe auch:
- MDN Web Docs zu Basic authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Axios, ein beliebter HTTP-Client für JavaScript: https://axios-http.com/
- Informationen zur sicheren Verwendung von Basisauthentifizierung: https://owasp.org/www-community/controls/Basic_Authentication
