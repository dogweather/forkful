---
title:                "Analisi di una data da una stringa"
aliases:
- /it/kotlin/parsing-a-date-from-a-string/
date:                  2024-02-03T19:14:38.811702-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisi di una data da una stringa"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è & Perché?
Analizzare una data da una stringa comporta la conversione del testo in un oggetto Date. Questa operazione è fondamentale per le applicazioni che interagiscono con date inserite dagli utenti o ottenute da set di dati esterni, consentendo una facile manipolazione e formattazione secondo le necessità.

## Come fare:
Kotlin supporta l'analisi delle date tramite il pacchetto `java.time`, introdotto in Java 8. Ecco un approccio semplice utilizzando `LocalDateTime` e uno schema specifico:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // Output: 2023-04-01T12:00
}
```

Per maggiore flessibilità, o per gestire date provenienti da fonti esterne come le API, si potrebbe utilizzare una libreria di terze parti come Joda-Time (anche se è meno comune ora con `java.time` che è robusto). Tuttavia, attenersi all'approccio moderno fornito dal JDK è preferito per la maggior parte delle applicazioni Kotlin.

Per analizzare una data in Kotlin senza utilizzare librerie di terze parti, è anche possibile utilizzare la classe `SimpleDateFormat` per le versioni precedenti a Java 8 o i livelli API Android che non supportano `java.time`:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // L'output varierà in base al tuo fuso orario, ad es., Sab Apr 01 12:00:00 GMT 2023
}
```

Ricorda di impostare sempre il fuso orario se lavori con `SimpleDateFormat` per evitare scostamenti imprevisti nelle date analizzate.
