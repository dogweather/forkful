---
title:                "Kotlin: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang ønsket å gjøre en streng mer leselig eller merkbar i koden din? Da kan det å bruke funksjonen for å kapitalisere en streng i Kotlin være løsningen! Ved å kapitalisere en streng, vil den første bokstaven i hvert ord bli stor.

## Hvordan

Det å kapitalisere en streng i Kotlin er lett og enkelt. Først må vi importere følgende pakke:

```Kotlin
import kotlinx.coroutines.*
```

Deretter kan vi bruke funksjonen `capitalize()` på en hvilken som helst streng:

```Kotlin
val tekst = "dette er en eksempeltekst"
println(tekst.capitalize())

// Output: Dette er en eksempeltekst
```

Vi kan også kapitalisere bare den første bokstaven i en streng ved å bruke funksjonen `capitalizeFirst()`:

```Kotlin
val navn = "olav"
println(navn.capitalizeFirst())

// Output: Olav
```

Hvis vi ønsker å kapitalisere en streng uavhengig av stor eller små bokstaver, kan vi bruke funksjonen `capitalizeWords()`:

```Kotlin
val tall = "eNToMMEr218TaLL"
println(tall.capitalizeWords())

// Output: Entommertall 218tall
```

## Dypdykk

Det å kapitalisere en streng kan være nyttig i mange forskjellige situasjoner. For eksempel, hvis du jobber med et program som tar inn brukerinput, kan det være lurt å kapitalisere inputen før den lagres eller brukes videre for å sikre at den er formatert på en konsistent måte. Det kan også være nyttig for å gjøre tekst mer leselig eller for å skille viktige ord eller navn i en streng.

En annen nyttig funksjon i Kotlin er `decapitalize()`, som gjør om første bokstav i en streng fra stor til liten. Dette kan være nyttig hvis du ønsker å formatere en streng til små bokstaver for å holde konsistens i koden din.

## Se også

- [String kapitalisering i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Standard bibliotek for Kotlin strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)
- [Kotlin referansedokumentasjon](https://kotlinlang.org/docs/reference/)