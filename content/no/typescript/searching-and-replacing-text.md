---
title:                "Søk og erstatt tekst"
html_title:           "TypeScript: Søk og erstatt tekst"
simple_title:         "Søk og erstatt tekst"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor er det viktig å kunne søke og erstatte tekst i programmering? Vel, tenk deg at du har en tekstfil med tusenvis av linjer med kode, og du må endre alle forekomster av en bestemt variabelnavn. I stedet for å manuelt endre hver eneste forekomst, kan du bruke søk og erstatt-funksjonen for å gjøre det raskere og enklere.

## Hvordan gjøre det

Søk og erstatt funksjonaliteten er tilgjengelig i de fleste tekstredigeringsprogrammer og IDEer, inkludert TypeScript. Her er en enkel måte å utføre en søk og erstatte operasjon i TypeScript:

```TypeScript
// Opprett en variabel med en verdi
let navn = "John";

// Bruk søk og erstatt-funksjonen for å endre variabelnavnet
navn = navn.replace("John", "Jane");

// Sjekk om endringen har blitt gjort ved å logge variabelen til konsollen
console.log(navn); // Output: Jane
```

Som du kan se, bruker vi `.replace()` -funksjonen på `navn` -variabelen og passerer inn den gamle verdien og den nye verdien. Funksjonen vil deretter søke etter alle forekomster av "John" og erstatte dem med "Jane". Dette er en rask og effektiv måte å endre variabelnavn eller tekst på.

## Dypdykk

I TypeScript og andre programmeringsspråk kan du bruke regulære uttrykk i `.replace()` -funksjonen for å gjøre søk og erstatt operasjoner mer avanserte. Regulære uttrykk er et kraftig verktøy for å søke og manipulere tekstbaserte data. La oss se på et eksempel:

```TypeScript
// Opprett en tekststreng med tall og bokstaver
let tekst = "123abc456def";

// Bruk et regulært uttrykk for å erstatte alle tall med bokstaven "x"
tekst = tekst.replace(/[0-9]/g, "x");

// Sjekk resultatet
console.log(tekst); // Output: xxxabcxxxdef
```

I dette eksempelet bruker vi `[0-9]` for å matche alle tall i tekststrengen, og `g` for å gjøre søket globalt (dvs. alle forekomster). Vi erstatter deretter alle treff med bokstaven "x". Dette kan være nyttig når du for eksempel skal fjerne alle tall fra en streng eller endre alle bokstaver til store bokstaver.

## Se også

- [MDN Web Docs -String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [TypeScript Official Handbook - Regular Expressions](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)