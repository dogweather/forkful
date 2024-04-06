---
date: 2024-01-20 18:02:43.682957-07:00
description: "Hvordan: Historisk sett ble HTTP Basic Authentication introdusert med\
  \ RFC 7617, og er en enkel, men mindre sikker autentiseringsform da det overf\xF8\
  rer\u2026"
lastmod: '2024-04-05T21:53:41.509205-06:00'
model: gpt-4-1106-preview
summary: "Historisk sett ble HTTP Basic Authentication introdusert med RFC 7617, og\
  \ er en enkel, men mindre sikker autentiseringsform da det overf\xF8rer brukernavn\
  \ og passord i \xE5pen tekst."
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

## Hvordan:
```TypeScript
import axios from 'axios';

// Encode your credentials
const username = 'brukernavn';
const password = 'passord';
const basicAuth = 'Basic ' + Buffer.from(username + ':' + password).toString('base64');

// Set up the HTTP request with basic authentication
axios.get('https://eksempel.no/data', {
  headers: { 'Authorization': basicAuth }
})
.then(response => {
  console.log('Data mottatt:', response.data);
})
.catch(error => {
  console.error('Det oppstod en feil:', error);
});
```
Output:
```
Data mottatt: { "noen": "data" }
```

## Deep Dive
Historisk sett ble HTTP Basic Authentication introdusert med RFC 7617, og er en enkel, men mindre sikker autentiseringsform da det overfører brukernavn og passord i åpen tekst. Det anbefales derfor å alltid bruke HTTPS med Basic auth.

Alternativer til Basic auth inkluderer OAuth, API-nøkler, og JWT-tokens (JSON Web Tokens), som alle tilbyr sterkere sikkerhet og mer kontroll.

Implementeringsdetaljer inkluderer at Basic auth bruker en `Authorization` header der brukernavn og passord er kodet med base64. Axios er et populært valg for å sende HTTP-forespørsler i TypeScript fordi det er løfteløst og håndterer promiser godt.

## Se Også
- [MDN Web Docs - Autorisasjonsheader](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Axios dokumentasjon](https://axios-http.com/docs/intro)
