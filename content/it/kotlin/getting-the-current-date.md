---
title:                "Ottenere la data attuale"
html_title:           "Kotlin: Ottenere la data attuale"
simple_title:         "Ottenere la data attuale"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perchè

In molti casi, potrebbe essere necessario ottenere la data corrente all'interno di un programma Kotlin. Ad esempio, per una applicazione di gestione delle attività, potresti voler visualizzare la data in cui una certa attività è stata creata o modificata.

## Come

Per ottenere la data corrente in Kotlin, è possibile utilizzare la classe `LocalDateTime` dal package `java.time`. Ecco un esempio di come stampare la data corrente in formato "dd/MM/yyyy" utilizzando la funzione `now` della classe `LocalDateTime`:

```Kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

val currentDateTime = LocalDateTime.now()
val dateFormat = DateTimeFormatter.ofPattern("dd/MM/yyyy")

println(currentDateTime.format(dateFormat))
```

L'output di questo codice sarà una stringa contenente la data corrente nel formato specificato, ad esempio "09/11/2021".

## Approfondimento

In Kotlin, la classe `LocalDateTime` è utilizzata per rappresentare una data e un'ora specifiche, senza considerare la timezone. Ciò significa che la classe non tiene traccia del fuso orario, che viene invece gestito dalla classe `ZoneId`. In questo modo, è possibile ottenere la data del fuso orario corrente utilizzando il metodo `now` della classe `LocalDateTime` specificando il timezone desiderato come parametro.

```Kotlin
val currentDateTime = LocalDateTime.now(ZoneId.of("Europe/Rome"))
```

Per ulteriori informazioni sulla gestione delle date e delle ore in Kotlin, ti consiglio di esplorare la documentazione ufficiale [qui](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-date-time/).

## Vedi Anche

- [Kotlin - Guida ufficiale](https://kotlinlang.org/docs/home.html)
- [Kotlin for Android - Tutorial su YouTube](https://www.youtube.com/playlist?list=PLrnPJCHvNZuDCyg4Usq2gHMzz6_CiyQO7)
- [Gestione delle date e delle ore in Java](https://www.baeldung.com/java-datetime-api)