---
date: 2024-01-20 17:47:35.845942-07:00
description: 'Hvordan: .'
lastmod: '2024-03-13T22:44:40.741172-06:00'
model: gpt-4-1106-preview
summary: .
title: "Finn lengden p\xE5 en streng"
weight: 7
---

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
