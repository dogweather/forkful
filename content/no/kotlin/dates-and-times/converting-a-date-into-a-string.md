---
date: 2024-01-20 17:36:44.306442-07:00
description: "Hvordan gj\xF8re det: ."
lastmod: '2024-03-13T22:44:40.762295-06:00'
model: gpt-4-1106-preview
summary: .
title: Konvertere en dato til en streng
weight: 28
---

## Hvordan gjøre det:
```kotlin
import java.text.SimpleDateFormat
import java.util.*

fun main() {
    val nå = Date()
    val dateFormat = SimpleDateFormat("dd.MM.yyyy HH:mm:ss")
    val datoSomStreng = dateFormat.format(nå)
    println(datoSomStreng)
}
```
Sample output:
```
31.03.2023 12:45:30
```

## Dypdykk
Historisk har dato- og tidsrepresentasjon variert mye mellom systemer. Kotlin bygger på Java's datohåndtering, og SimpleDateFormat er en måte å formatere datoer på som har vært med i Java siden 1.0. Alternativer inkluderer `DateTimeFormatter` fra Java 8, som er mer robust og trådsikkert. Implementasjonsdetaljer er viktige, spesielt med tanke på tidssoner og lokaliseringsinnstillinger som kan påvirke utdataene.

## Se også
- [SimpleDateFormat documentation](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Date and time classes in Java 8](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
