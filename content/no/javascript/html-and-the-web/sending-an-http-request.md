---
date: 2024-01-20 17:59:58.321267-07:00
description: "\xC5 sende en HTTP-foresp\xF8rsel lar en nettside hente data eller initiere\
  \ transaksjoner med servere over internett. Programmerere gj\xF8r dette for \xE5\
  \ bygge\u2026"
lastmod: '2024-03-11T00:14:14.780199-06:00'
model: gpt-4-1106-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel lar en nettside hente data eller initiere\
  \ transaksjoner med servere over internett. Programmerere gj\xF8r dette for \xE5\
  \ bygge\u2026"
title: "\xC5 sende en HTTP-foresp\xF8rsel"
---

{{< edit_this_page >}}

## What & Why?
Å sende en HTTP-forespørsel lar en nettside hente data eller initiere transaksjoner med servere over internett. Programmerere gjør dette for å bygge interaktive og dynamiske webapplikasjoner.

## How to:
Her er en enkel måte å sende `GET` og `POST` forespørsler ved hjelp av `fetch`:

```Javascript
// Send GET request
fetch('https://api.example.com/data')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Error:', error));

// Send POST request
fetch('https://api.example.com/data', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({ key: 'value' }),
})
.then(response => response.json())
.then(data => console.log('Success:', data))
.catch(error => console.error('Error:', error));
```
Eksempel output:
```
Success: { key: 'value' }
```

## Deep Dive:
Sending av HTTP-forespørsler stammer fra behovet for å kommunisere over weben. I tidligere dager ble XMLHTTPRequest ofte brukt, men `fetch` har nå blitt mer populært for sin enkle syntaks og løftesbaserte tilnærming. Mens `fetch` er inbygd og løser mange vanlige brukstilfeller, kan biblioteker som Axios tilby mer funksjonalitet og browserkompabilitet. Vi må også håndtere CORS (Cross-Origin Resource Sharing) når forespørsler sendes mellom ulike domener.

## See Also:
- [MDN Web Docs - Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [HTTP | MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [Axios GitHub Repository](https://github.com/axios/axios)
