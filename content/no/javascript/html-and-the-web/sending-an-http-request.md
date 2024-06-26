---
date: 2024-01-20 17:59:58.321267-07:00
description: "How to: Her er en enkel m\xE5te \xE5 sende `GET` og `POST` foresp\xF8\
  rsler ved hjelp av `fetch`."
lastmod: '2024-03-13T22:44:41.180813-06:00'
model: gpt-4-1106-preview
summary: "Her er en enkel m\xE5te \xE5 sende `GET` og `POST` foresp\xF8rsler ved hjelp\
  \ av `fetch`."
title: "\xC5 sende en HTTP-foresp\xF8rsel"
weight: 44
---

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
