---
title:    "Kotlin: Ottener la data corrente."
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché
Una delle funzionalità fondamentali di qualsiasi linguaggio di programmazione è la capacità di gestire date e orari. In questo articolo esploreremo come Kotlin ci aiuta a ottenere correttamente la data corrente in modo rapido e semplice.

## Come Fare
In Kotlin, possiamo utilizzare la classe `LocalDate` per ottenere la data corrente. Possiamo farlo in diversi modi, come mostrato nei seguenti esempi:

```Kotlin
val today = LocalDate.now()
println(today)
```
Questo ci darà l'output della data corrente nel formato "yyyy-MM-dd", ad esempio "2021-10-10".

Possiamo anche ottenere la data in un formato personalizzato utilizzando la classe `DateTimeFormatter`, come mostrato nel seguente esempio:

```Kotlin
val dateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
val today = LocalDate.now()

println(today.format(dateFormatter))
```
Questo ci darà l'output della data nel formato "dd/MM/yyyy", ad esempio "10/10/2021".

## Analisi Approfondita
In realtà, quando usiamo la funzione `LocalDate.now()`, stiamo effettivamente ottenendo l'istanza di `LocalDateTime` corrispondente alla data e all'ora correnti. Questo ci permette di accedere a informazioni più dettagliate, ad esempio il fuso orario e le fasce giornaliere.

Per ottenere l'ora corrente in un formato specifico come "HH:mm:ss", possiamo utilizzare il seguente codice:

```Kotlin
val timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")
val now = LocalTime.now()

println(now.format(timeFormatter))
```
Questo ci darà l'output dell'ora corrente nel formato "HH:mm:ss", ad esempio "15:30:23".

## Vedi Anche
- [Documentazione ufficiale di Kotlin su LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)
- [Documentazione ufficiale di Kotlin su LocalDateTime](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date-time/)
- [Esempi di utilizzo del formato delle date e delle ore in Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/dates-and-times.html)