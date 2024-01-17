---
title:                "Interpolering av tekststreng"
html_title:           "Kotlin: Interpolering av tekststreng"
simple_title:         "Interpolering av tekststreng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Interpolering av strenger betyr å sette inn variabler eller uttrykk i en annen streng for å generere en ny streng dynamisk. Dette er nyttig for å lage mer komplekse strenger uten å måtte skrive ut hele strengen manuelt. Programmere bruker dette for å lage mer effektiv og elegant kode.

## Hvordan:
Kotlin har en enkel syntaks for å interpolere en streng. Du kan bruke en dollar-tegn etterfulgt av en åpen parentes for å sette inn variabler eller uttrykk i en streng. La oss si at vi ønsker å generere en steg som sier "Hei Julia, ditt tall er 5". I stedet for å skrive ut hele strengen, kan vi bruke interpolering for å sette inn variablene Julia og 5 slik:

```Kotlin
val navn = "Julia"
val tall = 5

println("Hei $navn, ditt tall er $tall") 
```
Output:
```Hei Julia, ditt tall er 5```

Vi kan også legge til uttrykk i strengen ved å bruke krølleparenteser. La oss si at vi ønsker å regne ut et tall og inkludere svaret i en streng. Det kan gjøres slik:

```Kotlin
println("1+2 er ${1+2}")
```
Output:
```1+2 er 3```

## Dypdykk:
Interpolering av strenger begynte å bli brukt i programmeringsspråket Lisp på 1970-tallet. Dette konseptet har senere blitt implementert i ulike programmeringsspråk, inkludert Kotlin. En annen måte å generere komplekse strenger på er ved bruk av string formattering. Dette er også en effektiv måte å dynamisk generere strenger på, men kan være litt mer komplekst å implementere enn interpolering av strenger.

## Se også:
- [Offisiell dokumentasjon for Kotlin string interpolering](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)
- [Sammenligning av string formattering og string interpolering](https://www.freecodecamp.org/news/printf-format-strings-and-string-interpolation-comparison-and-cheat-sheet-735b6ddf321a/)