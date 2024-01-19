---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Sletting av tegn som samsvarer med et mønster i Javascript

## Hva & Hvorfor?
Å slette tegn som samsvarer med et mønster vil si at vi fjerner alle instanser av visse tegn fra en streng i koden vår. Dette kan være nyttig for å rense opp i data, for eksempel for å fjerne uønskede spesielle tegn.

## Hvordan:
Her er et eksempel på hvordan du kan bruke Javascript for å slette tegn som samsvarer med et mønster:

```Javascript
let str = "Hei! Dette er en% teststreng med * spesielle tegn.";
let nyStr = str.replace(/[!%*]/g, "");
console.log(nyStr);
```

Når du kjører denne koden, vil utdataene være "Hei Dette er en teststreng med spesielle tegn." som viser at alle forespurte spesialtegn har blitt fjernet.

## Dyp Dykk
Sletting av tegn som stemmer overens med et mønster i strenger ble introdusert i Javascript for å gi utviklere muligheten til å bearbeide tekst på en mer effektiv og kraftig måte. Ved hjelp av regulære uttrykk, kan utviklere bestemme hvilke tegn de ønsker å slette.

Det er et par ulike metoder vi kan bruke for å slette tegnet i Javascript, inkludert `slice`, `substr` og `substring`. Men den mest effektive og kraftige metoden er som du har sett, `replace`, sammen med regulære uttrykk.

Funksjonen `replace` med regulære uttrykk er ikke begrenset til bare å erstatte spesifikke tegn, men kan også brukes til å matche kompliserte mønstre av tegn og erstatte dem.

## Se Også
Du kan lære mer om regulære uttrykk og strengmanipulasjon i Javascript fra følgende kilder:

- Mozilla Developer Network's guide til [Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp) på w3schools.
- [Regexp](https://javascript.info/regexp-introduction) introduksjon på javascript.info.