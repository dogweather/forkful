---
date: 2024-01-26 03:49:55.933864-07:00
description: "Her er en liten smakebit p\xE5 feils\xF8king i Kotlin med IntelliJ IDEA\
  \ - Sherlock Holmes av IDEer: ```kotlin fun main() { val mysteryNumber = 42 var\
  \ guess = 0\u2026"
lastmod: '2024-03-13T22:44:40.755210-06:00'
model: gpt-4-0125-preview
summary: "Her er en liten smakebit p\xE5 feils\xF8king i Kotlin med IntelliJ IDEA\
  \ - Sherlock Holmes av IDEer: ```kotlin fun main() { val mysteryNumber = 42 var\
  \ guess = 0\u2026"
title: "\xC5 bruke en feils\xF8ker"
weight: 35
---

## Hvordan:
Her er en liten smakebit på feilsøking i Kotlin med IntelliJ IDEA - Sherlock Holmes av IDEer:

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("Gjett tallet: ")
        guess = readLine()?.toIntOrNull() ?: continue // Ignorer dårlige inndata

        // Sett et brytepunkt her for å se 'guess' i aksjon
        if (guess < mysteryNumber) {
            println("For lavt!")
        } else if (guess > mysteryNumber) {
            println("For høyt!")
        }
    }

    println("Du klarte det! Det mystiske tallet var $mysteryNumber")
}
```

Feilsøkerutskrift:
```
Gjett tallet: 
10
For lavt!
Gjett tallet: 
50
For høyt!
Gjett tallet: 
42
Du klarte det! Det mystiske tallet var 42
```

## Dypdykk
Feilsøkere har vært i spillet siden 50-tallet. Den gang var de ganske primitive, og feilsøking kunne handle mer om maskinvare enn programvare. I dag lar en feilsøker som den i IntelliJ IDEA oss sette brytepunkter, gå gjennom kode linje for linje, og inspisere tilstanden til variabler i vårt eget tempo.

Mens Intellij's feilsøker er super hendig for Kotlin, er det ikke den eneste fisken i havet. Det finnes en rekke alternativer som Logcat for Android-utvikling, eller kommandolinjeverktøy som jdb for minimalistene. Den underliggende magien her handler mest om JVM Tool Interface (JVMTI), som lar feilsøkere samhandle med Java Virtual Machine, og holder Kotlin-utviklere i loopen.

## Se også
- IntelliJ IDEA Feilsøkerdokumentasjon: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
