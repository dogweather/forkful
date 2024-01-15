---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Kotlin: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Se stai sviluppando un'applicazione che coinvolge date, potresti aver bisogno di calcolare una data futura o passata per mostrare informazioni rilevanti agli utenti. In questo articolo, vedremo come utilizzare il linguaggio di programmazione Kotlin per eseguire questo tipo di calcoli in modo semplice ed efficiente.

## How To

Per calcolare una data futura o passata in Kotlin, possiamo utilizzare la classe `Calendar` della libreria standard. Vediamo un esempio di come calcolare una data futura di un anno a partire dalla data corrente:

```Kotlin
// Ottieni la data corrente
val today = Calendar.getInstance()

// Aggiungi un anno
today.add(Calendar.YEAR, 1)

// Ottieni la nuova data
val futureDate = today.time

// Stampa il risultato
println("Data futura: $futureDate")
```

In questo esempio, stiamo ottenendo la data corrente utilizzando il metodo `getInstance()` della classe `Calendar`. Quindi, utilizziamo il metodo `add()` per aggiungere un anno alla data corrente e ottenere la nuova data. Infine, utilizziamo il metodo `time` per convertire la data in un oggetto `Date`. Il risultato verrà stampato nella console come una stringa.

Possiamo anche calcolare una data passata utilizzando il metodo `add()` con un valore numerico negativo, come ad esempio `-1` per sottrarre un anno. Inoltre, possiamo utilizzare altri campi come `Calendar.MONTH` o `Calendar.DAY_OF_MONTH` per aggiungere o sottrarre rispettivamente mesi o giorni alla data.

## Deep Dive

La classe `Calendar` offre molti altri metodi utili per lavorare con le date, come ad esempio `set()`, che ci consente di impostare una data specifica, e `get()`, che ci permette di ottenere il valore di un campo di una data. Possiamo anche utilizzare la classe `SimpleDateFormat` per formattare una data specifica in una stringa secondo un determinato pattern.

Inoltre, la libreria standard di Kotlin offre la classe `LocalDate` per la gestione delle date senza considerare il fuso orario, e la classe `ZonedDateTime` per rappresentare una data e un'ora con il fuso orario incluso.

In generale, quando si lavora con le date in Kotlin, è importante tenere conto dei possibili errori legati ai fusi orari e alle diverse convenzioni sulle date nei diversi paesi.

## See Also

- Documentazione ufficiale di Kotlin sulla classe `Calendar`: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.util.-calendar/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.util.-calendar/)
- Tutorial su come lavorare con date in Kotlin: [https://www.tutorialspoint.com/kotlin/kotlin_date_time.htm](https://www.tutorialspoint.com/kotlin/kotlin_date_time.htm)
- Articolo su come gestire fusi orari e date in Kotlin: [https://medium.com/@dbottillo/kotlin-dates-managing-time-in-a-multi-platform-way-5b8fbbb2a5e2](https://medium.com/@dbottillo/kotlin-dates-managing-time-in-a-multi-platform-way-5b8fbbb2a5e2)