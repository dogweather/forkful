---
date: 2024-01-20 17:33:19.388322-07:00
description: "Confrontare due date significa semplicemente verificare se sono uguali,\
  \ o stabilire quale viene prima o dopo. I programmatori lo fanno per gestire eventi,\u2026"
lastmod: '2024-03-13T22:44:43.403171-06:00'
model: gpt-4-1106-preview
summary: "Confrontare due date significa semplicemente verificare se sono uguali,\
  \ o stabilire quale viene prima o dopo. I programmatori lo fanno per gestire eventi,\u2026"
title: Confronto tra due date
weight: 27
---

## What & Why?
Confrontare due date significa semplicemente verificare se sono uguali, o stabilire quale viene prima o dopo. I programmatori lo fanno per gestire eventi, scadenze, e logiche relative al tempo.

## How to:
Kotlin rende il confronto tra date diretto grazie a `java.time.LocalDate`. Ecco come si fa:

```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2023, 3, 14)
    val date2 = LocalDate.of(2023, 3, 28)

    println(date1.isBefore(date2)) // true
    println(date1.isAfter(date2)) // false
    println(date1.isEqual(date2)) // false
}
```

Output:
```
true
false
false
```

## Deep Dive
Prima di `java.time`, introdotto in Java 8 e disponibile in Kotlin, avevamo `java.util.Date` e `java.util.Calendar`. Complesse e meno intuitive, spingevano gli sviluppatori a librerie esterne come Joda-Time.

Con `java.time` le cose si sono semplificate. Utilizza il concetto di immutabilità, rendendo le date sicure in contesti multithreading. Offre anche precisione di nanosecondi, coprendo la maggior parte dei bisogni.

Alternativamente, per operazioni più complesse ci si può affidare a librerie come ThreeTenABP su Android o facendo uso di APIs specifiche del framework.

Il confronto è basato sulla cronologia ISO e tiene conto dei fuse orari se si usa `ZonedDateTime` o `OffsetDateTime`.

## See Also
- Tutorial ufficiale Java Date and Time - [The Java™ Tutorials](https://docs.oracle.com/javase/tutorial/datetime/)
- ThreetenABP - [GitHub repository](https://github.com/JakeWharton/ThreeTenABP)
- Conversione tra vecchi e nuovi tipi - [StackOverflow discussion](https://stackoverflow.com/questions/19431234/converting-between-java-time-localdatetime-and-java-util-date)
