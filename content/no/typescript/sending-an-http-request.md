---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

---
## Hva & Hvorfor?

Å sende en HTTP (Hypertext Transfer Protocol) forespørsel er måten programmer kommuniserer med nettressurser - skrivende eller lesende data. Programmere gjør dette for å hente, legge til, oppdatere eller slette data fra eksterne servere.

## Hvordan Gjør Du Det:

Her er et eksempel på hvordan du kan sende en GET forespørsel ved hjelp av fetch API i TypeScript:

```TypeScript
async function getData(url: string): Promise<any> {
  const response = await fetch(url);
  
  if (!response.ok) {
    throw new Error(`HTTP error! status: ${response.status}`);
  } else {
    return await response.json();
  }
}

getData("https://api.example.com/data")
  .then(data => console.log(data))
  .catch(err => console.error('An error occurred:', err));
```

I dette eksemplet sender vi en forespørsel til `https://api.example.com/data` URL-en og logger dataene vi mottar.

## Dypdykk

HTTP protokollen ble skapt for å muliggjøre overføring av hypertext dokumenter over World Wide Web (WWW), og er i dag den mest generelle protokollen for dataoverføring på nettet. Det finnes andre protokoller som FTP for filoverføring, men HTTP er fortsatt standarden for kommunikasjon mellom klienter og servere på nettet.

Når det gjelder alternative måter å sende HTTP-forespørsler på, finnes det forskjellige biblioteker som axios, got, request, etc. som alle tilbyr forskjellige funksjoner og brukervennlighet.

I henhold til implementeringsdetaljer for HTTP-forespørsler, er det viktig å merke seg at fetch API returnerer et Promise som løser til Response-objektet som representerer svaret på forespørselen.

## Se Også
