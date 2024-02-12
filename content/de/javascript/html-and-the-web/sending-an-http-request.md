---
title:                "Einen HTTP-Request senden"
aliases:
- de/javascript/sending-an-http-request.md
date:                  2024-01-20T18:00:01.951548-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen HTTP-Request senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein HTTP-Request ermöglicht es deinem Code, mit einem Webserver zu sprechen; man holt oder sendet Daten. Programmierer brauchen das für APIs, um z.B. Wetterinfos abzurufen, oder Nutzerdaten zu speichern.

## So geht's:
```Javascript
// Einfacher GET Request mit fetch
fetch('https://api.example.com/data')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Fehler:', error));

// POST Request mit fetch
fetch('https://api.example.com/data', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({key: 'value'}),
})
  .then(response => response.json())
  .then(data => console.log('Erfolg:', data))
  .catch(error => console.error('Fehler:', error));
```
Sample Output:
```
Erfolg: {key: "value"}
```

## Deep Dive
Früher wurde XMLHttpRequest für HTTP-Requests verwendet, aber `fetch` ist neuer, verspricht basiert, und einfacher. Es gibt auch Libraries wie Axios oder das ältere jQuery.ajax. Bei komplexeren Anforderungen (z.B. Fortschritt Events) könnten Alternativen nützlich sein, aber für die meisten Fälle reicht `fetch`.

Die wichtigsten Aspekte beim Implementieren von HTTP-Requests in JavaScript:
- Die API/Server muss CORS (Cross-Origin Resource Sharing) erlauben, sonst blockiert der Browser den Aufruf.
- `fetch` gibt nicht direkt das JSON zurück, sondern ein Response-Objekt, aus dem man das JSON erst extrahieren muss.
- HTTP-Statuscodes geben an, ob und wie der Request erfolgreich war.
- Fehlerhandling ist wichtig, da vieles schiefgehen kann (Netzwerkprobleme, falsche Daten, Serverprobleme).

## Siehe Auch
- [MDN Web Docs on Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Fetch API Polyfill für ältere Browser](https://github.com/github/fetch)
- [Axios GitHub Repository](https://github.com/axios/axios)
