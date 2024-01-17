---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "TypeScript: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sending av en HTTP-forespørsel med grunnleggende autentisering er en måte å autentisere deg selv som bruker når du kommuniserer med en server. Dette er nyttig for programmerere fordi det hjelper dem med å sikre at bare autoriserte brukere har tilgang til bestemte ressurser på serveren.

## Hvordan:

```TypeScript
import * as http from 'http';

// Definerer forespørsel og autentiseringsinformasjon
const options = {
  host: 'localhost',
  port: 3000,
  path: '/example',
  // Legg til brukernavn og passord for autentisering
  auth: 'brukernavn:passord'
};

// Sender forespørselen med autentisering
http.get(options, (res) => {
  let data = '';

  // Leser responsen fra serveren
  res.on('data', (chunk) => {
    data += chunk;
  });

  // Endelig utskrift av responsen
  res.on('end', () => {
    console.log(data);
  });

}).on('error', (err) => {
  console.log("Error: " + err.message);
});
```

Output: Innehållet på den beskyttede ressursen fra serveren.

## Deep Dive:

Sending av en HTTP-forespørsel med grunnleggende autentisering er en av de enkleste formene for autentisering og har vært i bruk siden tidlig på 90-tallet. I tillegg til autentisering av brukeren, kan det også brukes til å autentisere applikasjoner når de kommuniserer med servere. Alternativene til grunnleggende autentisering inkluderer blant annet OAuth og JWT (JSON Web Tokens). Når du sender en HTTP-forespørsel med grunnleggende autentisering, vil brukernavnet og passordet bli kodet som Base64 før det sendes til serveren.

## Se også:

- [HTTP Dokumentasjon](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [Basic Access Authentication RFC](https://tools.ietf.org/html/rfc7617)
- [OAuth Dokumentasjon](https://oauth.net/2/)