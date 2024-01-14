---
title:                "Kotlin: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Il calcolo di una data nel futuro o nel passato può essere utile per pianificare eventi o tenere traccia di scadenze importanti. Inoltre, può essere interessante per scopi di apprendimento o divertimento.

## Come fare

Per calcolare una data nel futuro o nel passato in Kotlin, è possibile utilizzare la classe `LocalDate` del package `java.time`. Ad esempio, per ottenere la data di domani, è possibile utilizzare il metodo `plusDays()` come mostrato di seguito:

```Kotlin
val today = LocalDate.now()
val tomorrow = today.plusDays(1)

println(tomorrow) // Output: 2021-08-25
```

Inoltre, è possibile utilizzare altri metodi come `plusYears()`, `plusMonths()`, `minusDays()` per calcolare date in futuro o in passato in base alle proprie esigenze.

## Approfondimento

La classe `LocalDate` offre una serie di metodi utili per manipolare date nel modo desiderato. Ad esempio, è possibile ottenere il giorno della settimana di una specifica data utilizzando il metodo `getDayOfWeek()` o verificare se un anno è bisestile con il metodo `isLeapYear()`. Inoltre, è possibile utilizzare la classe `Period` per calcolare la differenza tra due date e ottenere il numero di giorni, mesi o anni trascorsi.

## Vedi anche

- [Documentazione ufficiale di Kotlin sul package `java.time`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time/index.html)
- [Tutorial su come calcolare date in Kotlin su Programiz](https://www.programiz.com/kotlin-programming/datetime)