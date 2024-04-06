---
date: 2024-01-20 18:02:06.432200-07:00
description: "How to: Basisauthentifizierung (Basic Authentication) ist ein alter\
  \ Mechanismus aus den fr\xFChen HTTP-Tagen, um Login-Daten sicher zu \xFCbermitteln.\u2026"
lastmod: '2024-04-05T21:53:56.153811-06:00'
model: gpt-4-1106-preview
summary: "Basisauthentifizierung (Basic Authentication) ist ein alter Mechanismus\
  \ aus den fr\xFChen HTTP-Tagen, um Login-Daten sicher zu \xFCbermitteln."
title: HTTP-Anfragen mit Basisauthentifizierung senden
weight: 45
---

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
