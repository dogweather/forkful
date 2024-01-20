---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Confrontare due date in programmazione significa decidere quale delle due date si verifica prima o se sono le stesse. Questo è vitale per la logica di molte applicazioni, come i task manager, le applicazioni di pianificazione o le applicazioni di navigazione in cui il calcolo del tempo è cruciale.

## Come fare:
Ecco un esempio semplice su come confrontare due date in Kotlin.

```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2022, 1, 1)
    val date2 = LocalDate.of(2023, 1, 1)

    when {
        date1.isBefore(date2) -> println("La date1 è prima della date2")
        date1.isAfter(date2) -> println("La date1 è dopo la date2")
        else -> println("La date1 è uguale alla date2")
    }
}
```

L'output sará:

```La date1 è prima della date2```

## Approfondimento
Il confronto delle date risale ai tempi in cui i computer erano appena iniziati. Tuttavia, il passaggio a Kotlin ha semplificato le cose grazie alla classe LocalDate di Java 8 e alle sue funzioni `isBefore` e `isAfter`.

Le alternative includono l'uso del vecchio `java.util.Date` e il confronto dei loro timestamp. Ma `java.time.LocalDate`, usato nel nostro esempio, è più facile da usare e più sicuro.

Nel dettaglio, le funzioni di confronto confrontano i campi delle date: prima l'anno, poi il mese, poi il giorno. Se tutte queste coincidono, allora le due date sono uguali.

## Vedi anche
Se vuoi saperne di più sul confronto delle date in Kotlin, dai un'occhiata a questi link:

1. Documentazione ufficiale di Kotlin: https://kotlinlang.org/docs/dates.html
2. Tutorial di Baeldung su LocalDate: https://www.baeldung.com/kotlin/date-time
3. Guida alla codifica di Java 8 LocalDate: https://www.javacodegeeks.com/2014/05/java-8-datetime-introducing-localdate-and-localtime.html