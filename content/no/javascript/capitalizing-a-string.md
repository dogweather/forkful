---
title:                "Gjør en streng stor"
html_title:           "Javascript: Gjør en streng stor"
simple_title:         "Gjør en streng stor"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å "capitalisere" en streng betyr å konvertere enkelte eller alle tegn i strengen til store bokstaver. Programmerere gjør dette for å forbedre lesbarheten eller for å oppfylle visse formatkrav.

## Hvordan:

Her er en kort kodebit for å "capitalisere" en streng i JavaScript:

```Javascript
function capitalizeString(str) {
    return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalizeString("programming is cool.")); \\ Output: "Programming is cool."
```
Hvis du vil gjøre alle ordene i strengen med store bokstaver, kan du bruke følgende kode:

```Javascript
function capitalizeWords(str) {
    return str.toLowerCase().split(' ').map(function(word) {
        return word.charAt(0).toUpperCase() + word.slice(1);
    }).join(' ');
}

console.log(capitalizeWords("programming is cool.")); \\ Output: "Programming Is Cool."
```

## Dyp Dykk

Å "capitalisere" en streng har vært en del av programmeringsspråk som JavaScript i mange år, og det har blitt en standard funksjon for strengmanipulasjon.

Det er andre metoder for å oppnå samme resultat, som bruk av CSS (Cascading Style Sheets) i nettutviklingskontekst. `text-transform: capitalize;` i CSS vil automatisk "capitalisere" alle ord i en tekst, uten å måtte manipulere selve strengen med JavaScript.

Når det gjelder implementeringsdetaljer, vil du kanskje merke at vi først konverterer alle tegnene til små bokstaver i den andre funksjonen (`capitalizeWords(str)`). Dette gjøres for å sikre at alle ordene starter med små bokstaver, slik at vi deretter bare kan konvertere det første tegnet til stort bokstav.