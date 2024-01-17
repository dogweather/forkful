---
title:                "Å laste ned en nettside"
html_title:           "Javascript: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor? 
Nedlasting av en nettside er prosessen med å hente informasjon og / eller ressurser fra en nettside og lagre dem lokalt på datamaskinen din. Dette gjøres vanligvis av programmerere for å få tilgang til og manipulere data fra nettsider for å bruke i sine egne applikasjoner eller for å automatisere oppgaver.

## Hvordan: 
 For å laste ned en nettside i Javascript, kan du bruke "fetch" -funksjonen som brukes til å hente data fra en ekstern ressurs. Her er et eksempel på hvordan du kan laste ned en nettside og skrive ut innholdet i konsollen:

```Javascript
fetch('https://www.example.com')
  .then(response => response.text())
  .then(data => console.log(data));
```

Etter å ha kjørt dette stykket kode, vil du se innholdet på nettsiden printet ut i konsollen din.

## Dypdykk: 
Nedlasting av nettsider har vært en viktig del av utvikling og automatisering av oppgaver på internett i lang tid. Tidligere ble det vanligvis gjort med klient-servantprogrammer som Wget og Curl. I dag er det mange Javascript-biblioteker som også gjør dette mulig, for eksempel "axios" og "request". Implementasjonen av disse bibliotekene kan variere, så det er viktig å lese dokumentasjonen for å forstå hvordan de fungerer.

## Se også: 
- [Fetch API dokumentasjon](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Axios GitHub repository](https://github.com/axios/axios)
- [Request GitHub repository](https://github.com/request/request)