---
title:                "Å bruke regulære uttrykk"
html_title:           "Gleam: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Vi bruker tekstbehandling hver dag for å kommunisere og uttrykke oss. Men noen ganger trenger vi mer kontroll over tekstbehandlingen, som å finne eller erstatte bestemte mønstre. Det er her regulære uttrykk kommer inn - et kraftig verktøy for å søke, filtrere og manipulere tekstbasert data.

## Hvordan

Gleam er et funksjonelt programmeringsspråk utviklet for å være enkelt å bruke og leselig. For å bruke regulære uttrykk i Gleam, bruker vi "regex" biblioteket. La oss se på noen eksempler for å forstå hvordan det fungerer:

```Gleam
let regex = regex.new("gleam")
let string = "Gleam er et fantastisk programmeringsspråk"
regex.match(string) // Output: found
```
Her oppretter vi et nytt regulært uttrykk som leter etter strengen "gleam". Deretter sjekker vi om den finnes i en annen streng, og får som output "found".

```Gleam
let regex = regex.new("[0-9]+")
let string = "2021 er et fantastisk år"
regex.replace(string, "42") // Output: 42 er et fantastisk år
```
I dette eksempelet erstatter vi alle tall i en streng med "42". Resultatet blir "42 er et fantastisk år".

## Dykk dypere

Regulære uttrykk er et kraftig verktøy som gir oss muligheten til å finne og manipulere tekstbaserte data på en effektiv måte. Men det er et omfattende emne med mange forskjellige aspekter å utforske. Når du blir mer fortrolig med Gleam og regex, kan du se på avanserte funksjoner som valgfrie deler, tilbakeføringer og uttrykk med variabler.

## Se også

- [Gleam Hjemmeside](https://gleam.run)
- [regex bibliotek dokumentasjon](https://gleam.run/lib/regex.scm.html)