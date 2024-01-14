---
title:                "Kotlin: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Å bruke regulære uttrykk kan bidra til å gjøre programmering mer effektivt og mindre tidkrevende. Ved å bruke regulære uttrykk kan vi enklere søke og manipulere tekster basert på mønstre, istedenfor å måtte bruke lange og komplekse kodeblokker for å oppnå det samme resultatet.

## Hvordan

For å bruke regulære uttrykk i Kotlin, kan vi bruke den innebygde klassen `Regex`. For å søke etter et bestemt mønster i en tekst, kan vi bruke funksjonen `find()` og for å erstatte et mønster med en annen tekst, kan vi bruke `replace()`.

Eksempel:

```Kotlin

val tekst = "Hei! Jeg elsker å kode i Kotlin"
val mønster = Regex("kode")
val funnet = mønster.find(tekst)
println(funnet?.value) // vil skrive ut "kode"

val erstattettekst = mønster.replace(tekst, "programmere")
println(erstattettekst) // vil skrive ut "Hei! Jeg elsker å programmere i Kotlin"
```
## Dypdykk

Regulære uttrykk har et rikt utvalg av metoder og operatører som kan hjelpe oss med å søke og manipulere tekster på en enkel og effektiv måte. Noen av de vanligste inkluderer:

- `find()`: brukes til å finne et mønster i en tekst og returnerer det første treffet.
- `findAll()`: brukes til å finne alle treff på et gitt mønster i en tekst og returnerer en liste over alle treffene.
- `replace()`: brukes til å erstatte et mønster med en annen tekst.
- `split()`: brukes til å dele en tekst basert på et gitt mønster og returnerer en liste over de delte delene.

For mer informasjon om hvordan du bruker regulære uttrykk i Kotlin, kan du se dokumentasjonen på Kotlin-streams [her](https://kotlinlang.org/docs/sequences.html#regular-expressions).

## Se også

[Offisiell Kotlin-dokumentasjon](https://kotlinlang.org/docs/reference/regular-expressions.html)
[Tutorial: Bruke regulære uttrykk i Kotlin](https://www.baeldung.com/kotlin-regex)