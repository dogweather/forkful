---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:37:10.073857-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Il parsing di una data da stringa consiste nel convertire testo che rappresenta una data in un formato utilizzabile per calcoli e manipolazioni. I programmatori lo fanno per interagire con date in formati differenti, per validazione o per immagazzinare informazioni in modo consistente.

## How to:
```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2023-04-01"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val date = LocalDate.parse(dateString, formatter)

    println(date) // Output: 2023-04-01
}
```

## Deep Dive
Parsing di date è una pratica comune in programmazione sin dalle origini. In Kotlin, la libreria `java.time` (introdotto in Java 8 e disponibile in Kotlin), permette un handling robusto di date e orari.

Alternativamente, si può usare la vecchia libreria `java.util.Date`, ma è meno consigliata per i suoi problemi di design, come la mutabilità e i problemi di thread safety. Prima di Java 8, librerie di terze parti come Joda-Time erano uno standard per la gestione del tempo in modo più elegante e meno problematico.

Per quanto riguarda l’implementazione, usare il formato standard ISO (come `yyyy-MM-dd`) aiuta nella comunicazione di date fra sistemi diversi. Si può personalizzare il formato usando `DateTimeFormatter`.

## See Also
- [Oracle JavaDocs - DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Baeldung tutorial on java.time](https://www.baeldung.com/java-8-date-time-intro)
