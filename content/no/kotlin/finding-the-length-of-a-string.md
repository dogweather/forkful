---
title:                "Finn lengden på en streng"
date:                  2024-01-20T17:47:35.845942-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finn lengden på en streng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden på en streng betyr å telle antall tegn i den. Vi gjør det for å validere inndata, skjære opp tekst, eller bare for å holde oversikt over størrelsen på informasjonen vi arbeider med.

## Hvordan:
```kotlin
fun main() {
    val example = "Hei, Norge!"
    println("Lengden av strengen er: ${example.length}")
}
```
Output:
```
Lengden av strengen er: 11
```

## Dypdykk
I databehandlingens barndom, når hvert byte teller, var det avgjørende å vite nøyaktig hvor lang en streng var for å unngå å sløse med plass. I moderne Kotlin, bruker vi `.length` egenskapen som kommer direkte fra Java String-klasse arven. Alternativene inkluderer å bruke en loop for å manuelt telle tegn, men hvorfor gjøre livet komplisert? Under hetten til `.length` kaller Kotlin Java-funksjonen, som effektivt er et felt som representerer antall `char` verdier brukt til å opprette strengen.

## Se Også
- Kotlin dokumentasjon på strenger: [Kotlin String - Kotlin Programming Language](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Java dokumentasjon for bedre forståelse av arv: [Class String (java.lang)](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
