---
title:                "HTTP-Anfragen mit Basisauthentifizierung senden"
date:                  2024-01-20T18:02:49.610504-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-Anfragen mit Basisauthentifizierung senden"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen mit Basisauthentifizierung schicken Nutzername und Passwort codiert im Header, um Zugriff auf geschützte Ressourcen zu erhalten. Programmierer nutzen das für einfache Authentifizierungsverfahren, wenn schneller Zugriff wichtiger ist als starke Sicherheit.

## So geht's:
Verwenden Sie axios oder eine native fetch-Implementierung in TypeScript, um Basisauthentifizierung durchzuführen. Axios-Beispiel:

```typescript
import axios from 'axios';

const url = 'https://meine-api.de/daten';
const username = 'meinUsername';
const password = 'meinPasswort';
const basicAuth = 'Basic ' + Buffer.from(username + ':' + password).toString('base64');

axios.get(url, {
  headers: {
    'Authorization': basicAuth
  }
})
.then(response => {
  console.log(response.data); // Erfolgreiche Antwort hier handhaben
})
.catch(error => {
  console.error('Authentifizierungsfehler:', error); // Fehlerbehandlung
});
```

Verwendung mit `fetch`:

```typescript
const url = 'https://meine-api.de/daten';
const username = 'meinUsername';
const password = 'meinPasswort';
const basicAuth = 'Basic ' + btoa(username + ':' + password);

fetch(url, {
  method: 'GET',
  headers: {
    'Authorization': basicAuth
  }
})
.then(response => response.json())
.then(data => {
  console.log(data); // Erfolgreiche Antwort hier handhaben
})
.catch(error => {
  console.error('Authentifizierungsfehler:', error); // Fehlerbehandlung
});
```

## Vertiefung
Basisauthentifizierung ist eine altbewährte Methode, die aufgrund ihrer Einfachheit in vielen alten und einigen modernen Systemen verwendet wird. Heutzutage gibt es sicherere Alternativen, wie OAuth und Tokenbasierte Authentifizierung, die besonders für öffentliche APIs bevorzugt werden. Die Basisauthentifizierung sendet Anmeldedaten in Base64 enkodiert, was nicht als sicher gilt, wenn es nicht über HTTPS erfolgt.

## Siehe auch:
- MDN Web Docs zur Authentifizierung: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Axios Dokumentation: https://github.com/axios/axios
- fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API