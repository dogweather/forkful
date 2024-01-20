---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La conversione di una data da una stringa è un processo nel quale trasformiamo una stringa in un oggetto data. Questo viene fatto dai programmatori per manipolare e utilizzare meglio le date in un contesto di programmazione.

## Come fare:

Ecco un esempio su come fare parsing di una stringa in una data in Kotlin:

```kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
  val formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
  val stringData = "21.12.2022"
  val data = LocalDate.parse(stringData, formatter)

  println(data)  // Stampa: 2022-12-21
}
```

In questo esempio, definiamo un formato di data (dd.MM.yyyy) e una stringa di data. Usiamo il metodo `LocalDate.parse()` per convertire la stringa in un oggetto data.

## Approfondisci:

Nel contesto storico, prima dell'introduzione del modulo `java.time.*` con Java 8, i programmatori usavano le classi `java.util.Date` e `java.text.SimpleDateFormat` per effettuare il parsing delle stringhe data. Ma queste classi erano poco pratiche e presentavano diversi problemi, tra cui thread-safety.

Una alternativa notevole al `LocalDate.parse()` nelle versioni più recenti di Kotlin è l'utilizzo degli standard ISO per rappresentare le date. Ad esempio, possiamo fare parsing di date ISO senza dover specificare un formato:

```kotlin
import java.time.LocalDate

fun main() {
  val stringData = "2022-12-10"
  val data = LocalDate.parse(stringData)

  println(data)  // Stampa: 2022-12-10
}
```

Dettagli di implementazione: quando si effettua il parsing di una data, è importante specificare il formato corretto. Se il formato non matcha la stringa data, si otterrà un errore di runtime.

## Vedi anche:

2. [Java 8 Date/Time API - Tutorial by Oracle](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
3. [Guide to the Date and Time API in Java](https://www.baeldung.com/java-8-date-time-intro)