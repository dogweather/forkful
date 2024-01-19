---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en dato til en streng (string) betyr å omforme datoobjektets format til en lesbar tekststreng. Dette gjør vi for å kunne vise datoen i et bestemt format, eller for å lettere behandle datoen som tekst i applikasjoner.

## Hvordan:
Se kodestumpene og utskriftseksemplene nedenfor for å forstå hvordan det fungerer:

```TypeScript
let dato = new Date();
let strengDato = dato.toISOString();
console.log(strengDato);
// Output: "2022-09-29T21:30:00.000Z"
```
Her brukes JavaScripts innebygde Date-objekt og dens metode `toISOString()` for å konvertere en dato til en ISO 8601-streng.

## Dypdykk: 
Konvertering av datoer til strenger har lange historiske røtter, og går tilbake til tidlig programmering og databehandling. Før støtte for datoobjekter var utbredt, ble datoer oftest håndtert som strenger.

Det finnes mange måter å konvertere en dato til en streng på i TypeScript. `toISOString()` er bare en av flere metoder tilgjengelige. Alternativer inkluderer `toDateString()`, som gir en mer lesbar streng, `toLocaleDateString()`, som gir en lokaliserbar dato streng, og `toUTCString()`, som gir en UTC-dato streng.

Imidlertid er `toISOString()` spesiell ettersom den gir en standardisert format (ISO 8601) som kan håndteres konsekvent på tvers av ulike programmeringsspråk og plattformer.

## Se Også:
Formatering av dato og tid: https://developer.mozilla.org/no/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString

Bruke Date-objekter: https://developer.mozilla.org/no/docs/Web/JavaScript/Reference/Global_Objects/Date

TypeScript dokumentasjon: https://www.typescriptlang.org/docs/