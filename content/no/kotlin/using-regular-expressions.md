---
title:                "Bruk av regulære uttrykk"
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk, eller regex, lar deg lete etter mønstre i tekst. De er kraftige for å validere, analysere eller endre strenger, og sparer tid ved å forenkle komplekse tekstoperasjoner.

## Slik går du frem:
```kotlin
fun main() {
    val tekst = "Hei, dette er en test - kontakt oss på epost: eksempel@eksempel.no."
    val emailRegex = "\\S+@\\S+\\.\\S+".toRegex()

    val funnetEpost = emailRegex.find(tekst)?.value
    println(funnetEpost)  // Utskrift: eksempel@eksempel.no
}
```

## Dypdykk:
Regex ble populært på 1960-tallet med utviklingen av programmeringsspråket Perl. Alternativer til regex inkluderer tekstbehandlingsbiblioteker eller innebygde strengfunksjoner, men disse kan være mindre fleksible. I Kotlin implementeres regex gjennom `Regex`-klassen, som tilbyr funksjoner som `find`, `matchEntire`, og `replace`.

## Se Også:
- Kotlin Dokumentasjon på Regulære Uttrykk: [Regex class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/index.html)
- Regex tester for å eksperimentere raskt: [regex101](https://regex101.com/)
- På dybden forståelse av regex: [Regular Expressions.info](https://www.regular-expressions.info/)