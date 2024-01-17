---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Javascript: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Konvertering av en streng til små bokstaver er en vanlig operasjon i Javascript-programmering. Dette betyr rett og slett å endre alle bokstavene i en streng til små bokstaver. Dette er ofte gjort for å sammenligne og behandle tekst på en enklere måte.

## Slik gjør du det:

```Javascript
// Opprett en variabel med en streng:
let tekst = "JEG LIKER Å PROGRAMMERE";

// Bruk "toLowerCase" -funksjonen for å konvertere til små bokstaver:
let konvertertTekst = tekst.toLowerCase();

// Skriv ut resultatet:
console.log(konvertertTekst);

// Output: "jeg liker å programmere"
```

## Dypdykk:

Konvertering av strenger til små bokstaver har vært en del av programmeringsspråk i lang tid. Dette konseptet kommer fra ASCII (American Standard Code for Information Interchange) -standarden som ble utviklet på 1960-tallet. Det er også flere alternative måter å konvertere tekst til små bokstaver, som for eksempel "toLower" -funksjonen i Java eller "strtolower" -funksjonen i PHP.

I Javascript er det også en lignende funksjon kalt "toUpperCase", som konverterer en streng til store bokstaver. Det er også mulig å konvertere en enkelt bokstav til små eller store bokstaver ved hjelp av "toLowerCase" eller "toUpperCase" funksjoner.

## Se også:

- [MDN web docs - toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [ASCII - American Standard Code for Information Interchange](https://www.ascii-code.com/)
- [W3Schools - toLowerCase() Function](https://www.w3schools.com/jsref/jsref_tolowercase.asp)