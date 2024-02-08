---
title:                "Confronto tra due date"
aliases:
- it/kotlin/comparing-two-dates.md
date:                  2024-01-20T17:33:19.388322-07:00
model:                 gpt-4-1106-preview
simple_title:         "Confronto tra due date"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

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
