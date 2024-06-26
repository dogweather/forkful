---
date: 2024-01-20 17:31:30.183884-07:00
description: 'How to: Kotlin rende il calcolo delle date semplice con la libreria
  `java.time`.'
lastmod: '2024-03-13T22:44:43.404123-06:00'
model: gpt-4-1106-preview
summary: Kotlin rende il calcolo delle date semplice con la libreria `java.time`.
title: Calcolo di una data futura o passata
weight: 26
---

## How to:
Kotlin rende il calcolo delle date semplice con la libreria `java.time`.

```kotlin
import java.time.LocalDate
import java.time.temporal.ChronoUnit

fun main() {
    val oggi = LocalDate.now()
    val dueSettimaneFa = oggi.minus(2, ChronoUnit.WEEKS)
    val inTreMesi = oggi.plusMonths(3)

    println("Oggi: $oggi")
    println("Due settimane fa: $dueSettimaneFa")
    println("In tre mesi: $inTreMesi")
}
```
**Output:**
```
Oggi: 2023-04-01
Due settimane fa: 2023-03-18
In tre mesi: 2023-07-01
```

## Deep Dive
Prima dell'introduzione di `java.time` in Java 8, si usavano `java.util.Date` e `java.util.Calendar`, che erano meno intuitivi e sicuri. `java.time` fornisce API concisi e thread-safe per gestire le date.

Oltre a `plus` e `minus`, possiamo usare altre funzioni come `withDayOfMonth` per impostare un specifico giorno del mese. Considera il fuso orario quando lavori con system clock globali.

## See Also
- Java 8 Date and Time guide: [Oracle Docs](https://docs.oracle.com/javase/tutorial/datetime/)
- `java.time` package overview: [Java Platform, SE 8](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Kotlin documentation: [Kotlinlang](https://kotlinlang.org/docs/reference/)
