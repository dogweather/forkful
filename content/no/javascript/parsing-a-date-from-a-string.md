---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing av datoer fra strenger i JavaScript

## Hva & Hvorfor?

Parsing av datoer fra strenger handler om å omforme strengdata til et dato-objekt i programmering. Vi gjør dette for å kunne manipulere og bruke disse datoverdiene i vår kode på en enklere og mer effektiv måte.

## Hvordan gjøre det:

Her er noen grunnleggende kodeeksempler om hvordan vi kan parse en dato fra en streng i JavaScript.

```Javascript
let datoStreng = "2022-02-22";
let parsDato = new Date(datoStreng);
console.log(parsDato);
```

Når det kjøres, vil koden over utskrive en dato-objekt for 22. februar 2022.

```Javascript
let datoStreng = "2022-02-22T14:20";
let parsDato = new Date(datoStreng);
console.log(parsDato);
```

Koden over vil utskrive en dato-objekt som inkluderer tiden 14:20.

## Dybdeplunge

*Historisk kontekst*: Tidligere var det ganske besværlig å håndtere datoer og tider i JavaScript, men med ECMAScript 5 (ES5) og senere versjoner, har det blitt langt enklere å manipulere datoer og tider.

*Alternativer*: Andre måter å parse datoer på er blant annet bruk av Date.parse() metoden eller bibliotek som Moment.js, som gir mer fleksible og kraftige verktøy for dato/tidshåndtering.

*Gjennomføring av detaljer*: Når du bruker JavaScript's Date constructor til å parse en streng, er det viktig å være klar over at den tolker datoen som UTC. Dette kan føre til utilsiktet oppførsel hvis du jobber i forskjellige tidssoner.

## Se også:

2. [JavaScript Date Objects](https://www.w3schools.com/js/js_date_methods.asp)
3. [Moment.js](https://momentjs.com/)