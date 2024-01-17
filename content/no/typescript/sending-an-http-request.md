---
title:                "Å sende en http-forespørsel"
html_title:           "TypeScript: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Sending av en HTTP-forespørsel er en måte for programmerere å kommunisere med en annen datamaskin eller server ved hjelp av internett. Dette lar dem få tilgang til og hente data eller informasjon fra en annen kilde. Programmere gjør dette for å kunne bygge komplekse applikasjoner, hente oppdaterte data og kommunisere med tredjeparts API-er.

# Hvordan:
```TypeScript
// Eksempel på en HTTP-forespørsel ved hjelp av innbygget Node.js modulet

const https = require('https');

https.get('https://jsonplaceholder.typicode.com/todos/1', (response) => {
  let data = '';
  response.on('data', (chunk) => {
    data += chunk;
  });

  response.on('end', () => {
    console.log(JSON.parse(data));
  });

}).on("error", (err) => {
  console.log("Feil: " + err.message);
});

// Eksempel på resultat:
/* 
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
*/

```

# Dypdykk:
HTTP-forespørsler har vært en vanlig måte å kommunisere med andre maskiner og servere på siden internettets tidlige dager. I dag finnes det alternativer som GraphQL og WebSockets som gir mer fleksibilitet og funksjonalitet. Implementasjon av HTTP-forespørsler i TypeScript er relativt enkel ved hjelp av biblioteker som Axios og Fetch.

# Se også:
- [What is an HTTP request? (Engelsk)](https://www.cloudflare.com/learning/performance/what-is-an-http-request/) 
- [Axios dokumentasjon (Engelsk)](https://github.com/axios/axios) 
- [Fetch API dokumentasjon (Engelsk)](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)