---
title:                "TypeScript: Sending en http-forespørsel"
simple_title:         "Sending en http-forespørsel"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor
HTTP forespørsler er en viktig del av nettutvikling, og det er et nødvendig konsept å forstå for alle som ønsker å skrive TypeScript-kode. Ved å sende en HTTP forespørsel kan du hente data fra et eksternt nettsted og bruke det i din egen applikasjon. Dette er spesielt nyttig når du integrerer med API-er eller ønsker å vise dynamisk innhold på en nettside.

## Hvordan
For å sende en HTTP forespørsel i TypeScript, må du bruke et tredjeparts bibliotek som Axios eller Fetch API. La oss ta en titt på et enkelt eksempel ved hjelp av Axios-biblioteket:

```TypeScript
import axios from 'axios';

// Sende GET forespørsel
axios.get('https://jsonplaceholder.typicode.com/users')
  .then(response => {
    // Hent data fra respons
    const users = response.data;
    // Gjør noe med dataen...
})
  .catch(error => {
    // Håndter eventuelle feil
    console.log(error);
});
```

I dette eksempelet bruker vi Axios for å sende en GET-forespørsel til en ekstern API-endepunkt som returnerer en liste over brukere. Deretter henter vi ut dataen fra responsen og kan gjøre hva vi vil med den. Du kan også sende andre typer HTTP forespørsler som POST, PUT eller DELETE ved å bruke forskjellige funksjoner fra Axios-biblioteket.

## Deep Dive
Når du sender en HTTP forespørsel, er det viktig å forstå responsen du mottar. Som standard returnerer Axios et JavaScript Promise-objekt, som gjør det mulig å kjede flere asynkrone handlinger sammen. Du kan også bruke funksjoner som `async` og `await` for å gjøre koden din mer lesbar og håndtere feil i en mer elegant måte. Det er også viktig å håndtere eventuelle feil som kan oppstå under forespørselen, slik som dårlig nettverksforbindelse eller ugyldig URL.

## Se også
- [Axios dokumentasjon](https://github.com/axios/axios)
- [Fetch API dokumentasjon](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Sammenligning mellom Axios og Fetch API](https://medium.com/@selvaganesh93/fetch-vs-axios-vs-request-axios-or-fetch-or-request-9f33afa846b5)