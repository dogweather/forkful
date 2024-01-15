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

## Hvorfor

`*Hvorfor skulle noen ønske å sende en HTTP forespørsel?*`

HTTP ("Hypertext Transfer Protocol") er et vanlig protokoll for å kommunisere mellom en klient og en server over internett. Ved å sende en HTTP forespørsel, kan en klient be om data fra en server og motta et svar tilbake. Dette er en nøkkelkomponent i nettapplikasjoner, da det tillater klienter å få tilgang til og bygge inn informasjon fra forskjellige servere.

## Hvordan gjøre det

For å sende en HTTP forespørsel i TypeScript, må du først initialisere en ny XMLHttpRequest-objektet.

```TypeScript
// Initialisere en ny XMLHttpRequest-objekt
let xhr = new XMLHttpRequest();
```

Deretter må du spesifisere hvilken type forespørsel du vil gjøre (GET, POST, PUT, osv.) og URL-en til serveren du vil kommunisere med.

```TypeScript
// Åpne en GET-forespørsel til en URL
xhr.open('GET', 'https://example.com');
```

Videre kan du spesifisere eventuelle parametere du vil sende med forespørselen ved hjelp av `send()`-metoden.

```TypeScript
// Sender en GET-forespørsel til en URL med parametere
xhr.send('param1=value&param2=value2');
```

Når responsen kommer tilbake fra serveren, kan du få tilgang til dataene ved hjelp av `responseText` eller `responseXML`-egenskapen til XMLHttpRequest-objektet.

```TypeScript
// Få tilgang til responsdataene
let data = xhr.responseText;
console.log(data);
```

## Dypdykk

I tillegg til å sende HTTP-forespørsler ved hjelp av XMLHttpRequest-objektet, kan du også bruke andre biblioteker som Axios, Fetch eller Superagent. Disse bibliotekene håndterer mye av den komplekse koden bak å sende en forespørsel og behandle responsen.

Det er også viktig å vite hvordan man håndterer HTTP-forespørsler på en sikker måte. Dette inkluderer å validere brukerinput for å unngå cross-site scripting (XSS)-angrep og bruke autentisering og autorisering for å sikre at kun autoriserte brukere kan få tilgang til data fra serveren.

## Se også

- [Axios dokumentasjon](https://github.com/axios/axios)
- [Fetch API dokumentasjon](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Superagent dokumentasjon](https://visionmedia.github.io/superagent/)