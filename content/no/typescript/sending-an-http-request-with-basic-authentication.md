---
title:                "Å sende en http forespørsel med grunnleggende autentisering"
html_title:           "TypeScript: Å sende en http forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http forespørsel med grunnleggende autentisering"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor 
Noen ganger, når du jobber med et nettsted eller en applikasjon, må du autentisere en bruker. Dette betyr at du må validere brukerens identitet før du gir tilgang til bestemte deler av nettstedet eller applikasjonen. En måte å gjøre dette på er ved hjelp av HTTP-forespørsler med grunnleggende autentisering. Dette er en enkel og effektiv måte å sikre at bare autoriserte brukere får tilgang til beskyttet informasjon.

## Hvordan 
La oss si at du har et nettsted der brukere kan logge inn og se sine personlige opplysninger. For å sikre at bare innloggede brukere kan få tilgang til disse opplysningene, kan du implementere HTTP-forespørsler med grunnleggende autentisering. Her er en enkel TypeScript-kode som viser hvordan du kan gjøre dette:

```TypeScript
import Axios, { AxiosResponse } from 'axios';

// Setter opp brukernavn og passord for autentisering
const username = 'brukernavn';
const password = 'passord';

// Setter opp Axios-klienten med autentiseringsheader
const axios = Axios.create({
  auth: {
    username,
    password,
  }
});

// Sender HTTP-forespørsel til en beskyttet URL
axios.get('https://example.com/personlig-info')
  .then((response: AxiosResponse) => {
    console.log(response.data); // utskrift av personlige opplysninger
  })
  .catch((error: AxiosError) => {
    console.log(error); // håndterer eventuelle feil
  })
  ```

Her vil Axios-klienten legge til autentiseringsheaderen i alle forespørsler, og serveren vil validere disse opplysningene for å gi tilgang til beskyttet informasjon.

## Dypdykk
Grunnleggende autentisering bruker en Base64-kodet streng som sendes i autentiseringsheaderen. Denne strengen består av brukernavn og passord separert med et kolon. Den blir deretter kodet og sendt som en del av HTTP-forespørselen. Serveren vil da dekode denne strengen og validere brukerinformasjonen.

En viktig ting å huske på er at grunnleggende autentisering ikke er en sikker måte å autentisere brukere på, da informasjonen kan enkelt dekodes av en hacker. Derfor bør det kun brukes i situasjoner der sikkerheten ikke er kritisk.

## Se også
- [Axios dokumentasjon](https://github.com/axios/axios)
- [Grunnleggende autentisering på MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)