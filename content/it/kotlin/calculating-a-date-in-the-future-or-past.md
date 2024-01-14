---
title:    "Kotlin: Calcolare una data nel futuro o passato"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Perché

Calcolare la data in futuro o passato può essere utile per varie ragioni, come pianificare un evento o tenere traccia del tempo trascorso da una determinata data.

## Come Fare

```Kotlin
// Date per 3 giorni fa
val oggi = LocalDate.now()
val treGiorniFa = oggi.minusDays(3)
println(treGiorniFa) // Output: 2021-01-08

// Date per 2 settimane fa e 2 settimane in futuro
val oggi = LocalDate.now()
val dueSettimaneFa = oggi.minusWeeks(2)
val dueSettimaneFuturo = oggi.plusWeeks(2)
println(dueSettimaneFa) // Output: 2021-01-01
println(dueSettimaneFuturo) // Output: 2021-01-22

// Date per un mese fa e un mese in futuro, specificando il formato
val oggi = LocalDate.now()
val unMeseFa = oggi.minusMonths(1)
val unMeseFuturo = oggi.plusMonths(1)
val formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")
println(unMeseFa.format(formatter)) // Output: 08-12-2020
println(unMeseFuturo.format(formatter)) // Output: 08-02-2021
```

## Approfondimento

Per calcolare una data nel futuro o nel passato, si utilizzano le funzioni `minus` e `plus` della classe `LocalDate` della libreria `java.time`. Queste funzioni possono prendere come parametro un numero di giorni, settimane o mesi da sottrarre o aggiungere alla data corrente. Inoltre, è possibile specificare un formato per visualizzare la data in modo personalizzato.

## Vedi Anche

- [Kotlin Docs - java.time.LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/index.html)
- [Documentazione di Kotlin sulla data e l'ora](https://kotlinlang.org/docs/datetime.html)
- [Libreria Java - java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)