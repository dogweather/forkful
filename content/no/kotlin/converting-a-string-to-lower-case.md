---
title:                "Kotlin: Konvertere en streng til små bokstaver."
simple_title:         "Konvertere en streng til små bokstaver."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver kan være nyttig for å sammenligne tekst, forenkle søk og generelt forbedre lesbarheten av koden din.

## Slik gjør du det

```Kotlin
val string = "HELLO WORLD"
val lowerCaseString = string.toLowerCase()
println(lowerCaseString)
```

Output: `hello world`

Her bruker vi `toLowerCase()`-funksjonen for å konvertere strengen `HELLO WORLD` til små bokstaver og deretter skrive den ut til konsollen.

## Dypdykk

Konvertering av en streng til små bokstaver er en vanlig oppgave i programmering. Det finnes flere måter å gjøre det på, men i Kotlin kan vi bruke den innebygde `toLowerCase()`-funksjonen for å gjøre det enkelt og effektivt. Denne funksjonen tar ikke bare hensyn til det latinske alfabetet, men også spesialtegn og diakritiske tegn.

En annen måte å konvertere en streng til små bokstaver på er å bruke `map()`-funksjonen og endre hvert tegn i strengen til små bokstaver individuelt. Her er et eksempel på hvordan dette kan gjøres:

```Kotlin
val string = "HELLO WORLD"
val lowerCaseString = string.map { it.lowercase() }.joinToString("")
println(lowerCaseString)
```

Output: `hello world`

Denne tilnærmingen kan være mer nyttig hvis du trenger å gjøre endringer på hvert tegn i strengen, for eksempel å erstatte spesialtegn med vanlige bokstaver.

## Se også

- [Kotlin - Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/)
- [Java - Converting String to lowercase](https://www.geeksforgeeks.org/java-converting-string-lower-case/)
- [Swift - Lowercased](https://developer.apple.com/documentation/swift/string/1689702-lowercased)