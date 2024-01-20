---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å finne lengden på en streng handler om å telle antall tegn i en gitt tekst (det kan være et ord, en setning, et avsnitt, osv.) I programmering gjør vi dette ofte for å kontrollere tekstdata, sette grenser, eller behandle tekst mer effektivt.

## Hvordan Gjøre Det:

Her er et enkelt eksempel på hvordan vi kan finne lengden på en streng i Javascript:

```Javascript
let tekst = "Hei, Norge!";
console.log(tekst.length);
```

Når du kjører denne koden, vil output være `11`, som er antallet tegn i teksten ("Hei, Norge!").

## Dypdykk

Javascript er ikke det første programmet som adapterte en metode for å finne lengden på en streng. Tidligere programmeringsspråk, for eksempel C og Python, hadde allerede lignende funksjonalitet. 

En alternativ måte å finne lengden på en streng i Javascript ville være å bruke en loop til å telle hvert tegn manuelt. Men `.length`-metoden er raskere og mye mer effektiv, så det er standard tilnærming.

Når det gjelder implementeringsdetaljer, så er `.length` egentlig en innebygd egenskap i Javascripts `String`-prototyp. Det betyr at hver gang du lager en streng, vil den automatisk ha en `.length`-egenskap.

## Se Også:

For å lære mer om strenger og deres handlinger i Javascript, kan du ta en titt på disse nyttige linkene:

2. [W3Schools' tutorial on Javascript String length](https://www.w3schools.com/jsref/jsref_length_string.asp)
3. [Exploring JS: Strings and their properties](http://exploringjs.com/es6/ch_strings.html)