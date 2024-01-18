---
title:                "Analisi di una data da una stringa"
html_title:           "Kotlin: Analisi di una data da una stringa"
simple_title:         "Analisi di una data da una stringa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Il parsing di una data da una stringa è il processo attraverso il quale i programmatori estraggono informazioni sulla data da una stringa di testo. Questo è utile quando la data è inserita in un formato diverso da quello utilizzato dal sistema. I programmatori spesso eseguono il parsing delle date per manipolarle e utilizzarle nei loro programmi.

## Come fare:

```Kotlin
import java.time.format.DateTimeFormatter
import java.time.LocalDate

fun main() {
  // Definizione della stringa contenente la data
  val dateString = "10/05/2020"
  // Utilizzo di un formatter per parsare la data
  val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
  // Creazione di un oggetto LocalDate
  val date = LocalDate.parse(dateString, formatter)
  // Visualizzazione della data nel formato desiderato
  println(date.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")))
}
```

L'output sarà: `10/05/2020`

## Approfondimento:

Il parsing delle date da una stringa è stato un problema comune per i programmatori in passato, poiché non c'erano strumenti specifici per eseguirlo facilmente. Tuttavia, con l'introduzione delle librerie e dei framework più moderni, è diventato un processo più semplice.  

In alternativa all'utilizzo del `DateTimeFormatter` di Kotlin, è possibile utilizzare altre librerie come Joda-Time o Java 8 Date and Time API. Inoltre, è importante prestare attenzione al formato della data in input e alla specifica lingua utilizzata, in quanto ciò può influire sulla riuscita del parsing.

Per quanto riguarda l'implementazione del parsing di una data da una stringa in Kotlin, il linguaggio offre una semplice e intuitiva sintassi che facilita il processo. Inoltre, utilizzando gli strumenti forniti dalla libreria standard di Kotlin, è possibile gestire facilmente eventuali errori durante il parsing della data.

## Vedi anche:

- [Guida ufficiale al parsing delle date con Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-date-time/index.html)
- [Documentazione di Joda-Time per il parsing delle date in Kotlin](https://www.joda.org/joda-time/index.html)
- [Tutorial su come utilizzare il Java 8 Date and Time API in Kotlin](https://howtodoinjava.com/kotlin/kotlin/java-8-datetime-api-support-kotlin-tutorial/)