---
title:                "Interpolering av en streng"
html_title:           "Gleam: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Interpolering av strenger er en måte for programmerere å sette sammen tekst og variabler på en enkel og effektiv måte. Det lar deg bygge dynamiske strenger som kan tilpasses etter behov, og er spesielt nyttig når du vil lage utskrifter eller tilbakemeldinger som må endres basert på ulike inndata.

## Slik gjør du:

For å interpolere en streng i Gleam, bruker du backticks-symbolet (`) rundt teksten din og # foran variablene dine. La oss prøve med et enkelt eksempel:

```Gleam
let navn = "Maria"
let alder = 28

```
Merk at variablene dine må definert før du bruker dem i en interpoleringsstreng.

For å sette sammen en streng, bruker du symbolet for interpolering (#) etterfulgt av variabelnavnet og skriver deretter koden din inni backticks:

```Gleam
`Hei, mitt navn er #navn og jeg er #alder år gammel.`
```
Denne koden vil resultere i følgende utskrift: `Hei, mitt navn er Maria og jeg er 28 år gammel.` 

Du kan også bruke så mange variabler du vil i en interpoleringsstreng, så lenge du følger samme syntaks. 

## Nærmere undersøkelse:

Interpolering av strenger har eksistert i programmering i en årrekke og brukes mye i flere forskjellige språk. En av de mest populære alternativene til Gleam er Python, som også tilbyr en enkel syntaks for strenginterpolering.

Gleam implementerer interpolering av strenger ved å erstatte variablene med den faktiske verdien under kjøring. Dette betyr at du kan bruke både numeriske og tekstbaserte variabler i en interpoleringsstreng.

## Se også:
Hvis du vil lære mer om interpolering av strenger i Gleam, kan du sjekke ut følgende ressurser:

- Offisiell dokumentasjon for Gleam sin syntaks for interpolering av strenger: [https://gleam.run/book/guide/interpolation.html](https://gleam.run/book/guide/interpolation.html)
- En artikkel om hvordan strenginterpolering fungerer i Python: [https://realpython.com/python-f-strings/](https://realpython.com/python-f-strings/)