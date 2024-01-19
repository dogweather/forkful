---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Tekstsammensetning, eller 'string concatenation', er hvor du slår sammen to eller flere strenger til en. Dette brukes av programmerere for å lage eller manipulere tekst i programmet.

## Hvordan:

```Kotlin
fun main() {
    val start = "Hei, "
    val end = "Norge!"
    val greeting = start + end
    println(greeting)  // Skriver ut "Hei, Norge!"
}
```

## Dyp Dykk:

Historisk sett har tekst-sammenslåing vært en fundamental funksjon i programmering. I Kotlin, kan du også bruke `StringBuilder` for effektiv sammenslåing: 

```Kotlin
fun main() {
    val sb = StringBuilder()
    sb.append("Hei, ").append("Norge!")
    println(sb.toString())  // Skriver ut "Hei, Norge!"
}
```

Alternativt kan du bruke `${}` for å slå sammen variabler og strenger; dette er kjent som 'string interpolation':

```Kotlin
fun main() {
    val land = "Norge"
    val greeting = "Hei, $land!"
    println(greeting)  // Skriver ut "Hei, Norge!"
}
```

## Se også:

1. [Kotlin dokumentasjon om tegnstrenger](https://kotlinlang.org/docs/reference/basic-types.html#strings)
2. [Kotlin 'StringBuilder' API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)
3. [Kotlin dokumentasjon om streng interpolasjon](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)