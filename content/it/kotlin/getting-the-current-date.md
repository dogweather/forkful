---
title:                "Kotlin: Ottenere la data corrente"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, come programmatori, abbiamo bisogno di conoscere la data e l'ora attuali per poter gestire i nostri dati in modo più efficiente o per creare funzionalità come un calendario o un timer. In questo caso, imparare a ottenere la data corrente tramite il linguaggio di programmazione Kotlin può essere molto utile.

## Come fare

Per ottenere la data corrente in Kotlin, possiamo utilizzare la classe `LocalDate` del package `java.time`. Dobbiamo prima importare questi package nel nostro codice:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter
```

Successivamente, possiamo creare un'istanza di `LocalDate` e utilizzare il metodo `now()` per ottenere la data corrente:

```Kotlin
val currentDate = LocalDate.now()
```

Possiamo anche specificare una zona temporale, come ad esempio l'UTC, utilizzando il metodo `now()` con un parametro:

```Kotlin
val currentDateUTC = LocalDate.now(ZoneOffset.UTC)
```

Per visualizzare la data in un determinato formato, possiamo utilizzare l'oggetto `DateTimeFormatter` e il suo metodo `format()`:

```Kotlin
val formattedDate = currentDate.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
```

L'output di questo codice sarà `27/09/2021`, in quanto utilizziamo il formato giorno/mese/anno. Ci sono molti altri formati disponibili, che possono essere esplorati nella documentazione ufficiale.

## Deep Dive

In Kotlin, le date sono rappresentate come istanze della classe `LocalDate`, che rappresenta una data senza alcuna informazione sul fuso orario. Se abbiamo bisogno di gestire anche il fuso orario, possiamo utilizzare la classe `ZonedDateTime`.

Inoltre, per eseguire operazioni come l'aggiunta o la sottrazione di giorni o mesi dalla data corrente, possiamo utilizzare i metodi `plus()` e `minus()`. Ad esempio, se vogliamo ottenere la data di ieri, possiamo utilizzare il seguente codice:

```Kotlin
val yesterday = LocalDate.now().minusDays(1)
```

Per ulteriori informazioni sulla gestione delle date in Kotlin, è possibile consultare la documentazione ufficiale.

## Vedi anche

- [Kotlin API - Classe LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)
- [Kotlin API - Classe ZonedDateTime](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-zoned-date-time/)
- [Documentazione ufficiale di Kotlin sulle date](https://kotlinlang.org/docs/datetime.html)