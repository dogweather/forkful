---
title:                "Å bruke regulære uttrykk"
html_title:           "Kotlin: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Regulære uttrykk er en måte for programvare å matche mønstre i strenger. Det er nyttig for å finne og manipulere data som følger et bestemt mønster, for eksempel e-postadresser, telefonnummer eller passord. Programmerere bruker regulære uttrykk for å gjøre databehandling og validering enklere og mer effektivt.

## Hvordan:

```Kotlin
// Finn alle e-postadresser i en streng
val tekst = "Hei! Kontakt meg på john@eksempel.com eller jane@eksempel.com"
val regex = Regex("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}")
val eposter = regex.findAll(tekst).map { it.value }.toList()

println(eposter)
// Output: [john@eksempel.com, jane@eksempel.com]
```

## Dypdykk:

Regulære uttrykk ble først introdusert i 1951 av matematikeren Stephen Cole Kleene. De ble senere implementert i programmeringsspråk som grep og Perl. I dag brukes regulære uttrykk i mange programmeringsspråk, inkludert Kotlin.

Alternativene til regulære uttrykk inkluderer strengmetoder og biblioteker som håndterer mønstergjenkjenning. Men disse kan være mindre effektive og mer komplekse enn regulære uttrykk. Implementasjonen av regulære uttrykk er også direkte koblet til uttrykksproblemet, som er et av de viktigste problemene i datavitenskap.

## Se også:

- [Kotlins dokumentasjon om regulære uttrykk](https://kotlinlang.org/docs/regular-expressions.html)
- [RegExr - online regulære uttrykkredigerer](https://regexr.com/)
- [Regulære uttrykk - en interaktiv veiledning](https://www.regular-expressions.info/tutorial.html)