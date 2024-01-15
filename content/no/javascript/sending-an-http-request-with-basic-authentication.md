---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Javascript: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til å bruke HTTP-forespørsler med grunnleggende autentisering i JavaScript.

En av hovedgrunnene er å sikre at kun autoriserte brukere har tilgang til spesifikke ressurser på en nettside eller server. Dette hjelper også til med å beskytte sensitiv informasjon, som for eksempel brukernavn og passord.

## Slik gjør du det

Sende en HTTP-forespørsel med grunnleggende autentisering i JavaScript er enkelt. Følg disse trinnene for å implementere det i koden din:

**1. Importer fetch-biblioteket**

I de nyere versjonene av JavaScript kan du bruke fetch funksjonen for å sende HTTP-forespørsler. Denne funksjonen er standard i moderne nettlesere, så du trenger ikke å installere noe ekstra bibliotek.

For å bruke fetch funksjonen, må du importere den i koden din ved hjelp av følgende kode:

```JavaScript
import fetch from 'fetch';
```

Merk: Hvis du bruker en eldre versjon av JavaScript, kan du bruke XMLHttpRequest (XHR) objektet for å sende HTTP-forespørsler.

**2. Bygg forespørselen**

Når du har importert fetch funksjonen, må du bygge opp HTTP-forespørselen. Dette gjøres ved å opprette et Request-objekt som inneholder URL-en til ressursen du vil ha tilgang til, samt metoden (GET, POST, PUT osv.) og eventuelle ekstra parametere.

Et eksempel på en HTTP-forespørsel med grunnleggende autentisering kan se slik ut:

```JavaScript
const request = new Request('https://example.com/brukere', {
  method: 'GET',
  headers: new Headers({
    'Authorization': 'Basic ' + btoa('brukernavn:passord')
  })
});
```

Her bruker vi btoa funksjonen til å kryptere brukernavnet og passordet vårt i Base64-format og legger dette som en del av autentiseringsheaderen.

**3. Utfør forespørselen**

Etter at du har bygget opp HTTP-forespørselen, er det på tide å sende den ved hjelp av fetch funksjonen. Dette kan gjøres med følgende kode:

```JavaScript
fetch(request)
.then(response => {
  // Gjør noe med svaret her
})
.catch(error => console.log(error));
```

Her bruker vi .then() metoden til å håndtere svaret fra serveren og .catch() metoden til å håndtere eventuelle feil som kan oppstå.

## Dypdykk

For å bedre forstå hvordan HTTP-forespørsler med grunnleggende autentisering fungerer, er det viktig å ha en grunnleggende forståelse av hvordan HTTP-protokollen fungerer.

HTTP står for Hypertext Transfer Protocol og er en protokoll som brukes til å overføre data over internett. Når en nettleser sender en forespørsel til en nettside, følger den en bestemt protokoll som inkluderer en forespørselsmetode (GET, POST, PUT osv.), en URL og en header med eventuelle ekstra parametere.

Ved å inkludere grunnleggende autentisering i denne headeren, kan nettleseren verifisere brukeren som sender forespørselen og gi tilgang til ressursen hvis autentiseringen er vellykket.

## Se også

- [HTTP-forespørsler med fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- [Grunnleggende autentisering i HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)