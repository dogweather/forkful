---
date: 2024-01-20 17:47:35.845942-07:00
description: "\xC5 finne lengden p\xE5 en streng betyr \xE5 telle antall tegn i den.\
  \ Vi gj\xF8r det for \xE5 validere inndata, skj\xE6re opp tekst, eller bare for\
  \ \xE5 holde oversikt over\u2026"
lastmod: 2024-02-19 22:04:59.988767
model: gpt-4-1106-preview
summary: "\xC5 finne lengden p\xE5 en streng betyr \xE5 telle antall tegn i den. Vi\
  \ gj\xF8r det for \xE5 validere inndata, skj\xE6re opp tekst, eller bare for \xE5\
  \ holde oversikt over\u2026"
title: "Finn lengden p\xE5 en streng"
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
