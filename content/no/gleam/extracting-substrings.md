---
title:                "Gleam: Ekstrahering av delstrenger"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å trekke ut delstrenger er en vanlig og nyttig operasjon i programmering, spesielt når du arbeider med tekst eller strenger. Det lar deg hente ut spesifikke deler av en streng basert på bestemte kriterier, som for eksempel en bestemt lengde eller et visst mønster. Dette kan være nyttig for å manipulere data, filtrere ut uønsket informasjon, eller for å lage mer dynamiske og tilpassede tekstelementer i programmene dine.

## Hvordan Gjøre Det

Å trekke ut substrings i Gleam er enkelt og intuitivt, takket være det funksjonelle språkets ryddige og leselige syntaks. La oss se på et eksempel:

```Gleam
let navn = "Ole Olesen"

let mellomnavn = String.slice(navn, 4, 8)

```

I denne koden definerer vi variabelen "navn" som en streng med verdien "Ole Olesen". Deretter bruker vi funksjonen "String.slice" for å trekke ut en del av denne strengen. Her er "4" og "8" start- og sluttindeksene vi vil bruke for å definere vår delstreng. I dette tilfellet vil "mellomnavn" ha verdien "Oles".

Det er også mulig å bruke en rekke andre funksjoner for å trekke ut substrings i Gleam, som for eksempel "String.left", "String.right" og "String.substr". Disse funksjonene lar deg gjøre ulike justeringer og hente ut deler av strengen basert på ulike kriterier.

## Dypdykk 

Å trekke ut substrings kan også gjøres ved hjelp av mønstre og regulære uttrykk. Dette kan gi deg større fleksibilitet og kontroll over hvilke deler av strengen du vil hente ut. For å bruke regulære uttrykk i Gleam, kan du importere modulen "Regex" og bruke funksjonen "Regex.find".

Det er også verdt å merke seg at når du trekker ut substrings i Gleam, returnerer funksjonene alltid en kopi av den opprinnelige strengen. Dette betyr at det originale strengobjektet ikke endres, og du må derfor lagre den resulterende delstrengen i en ny variabel for å bruke den senere i koden din.

## Se Også

- [Gleam sin offisielle dokumentasjon om strenger](http://gleam.run/documentation/std/string.html)
- [En guide til regulære uttrykk i Gleam](https://andy-whiteley.github.io/preview_area/language_documentation/code_examples/regex.html)