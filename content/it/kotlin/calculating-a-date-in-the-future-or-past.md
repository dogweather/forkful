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

# Calcolo delle date nel futuro e passato con Kotlin

## Cos'è e Perché?
Calcolare una data nel futuro o nel passato significa determinare una data specifica rispetto a un punto di riferimento temporale. I programmatori lo fanno per gestire eventi, programmare promemoria o per qualsiasi funzionalità che necessita una manipolazione delle date.

## Come Fare:

Ecco alcuni esempi su come calcolare una data nel futuro o nel passato in Kotlin:

Calcolo di una data in futuro:
```Kotlin
import java.time.LocalDate

fun main() {
    val oggi = LocalDate.now()
    val futuro = oggi.plusDays(10)
    println("La data in futuro di 10 giorni da oggi è: $futuro")
}
```
OUTPUT:
// La data in futuro di 10 giorni da oggi è: 2023-07-12

Calcolo di una data nel passato:
```Kotlin
import java.time.LocalDate

fun main() {
    val oggi = LocalDate.now()
    val passato = oggi.minusYears(2)
    println("La data due anni fa da oggi era: $passato")
}
```
OUTPUT:
// La data due anni fa da oggi era: 2021-07-02

## Approfondimento

**1. Contesto storico:** Java ha introdotto un quadro temporale moderno con la versione 8, sulla quale Kotlin si appoggia. Questo quadro temporale risolve molti problemi presenti nelle API temporali precedenti.

**2. Alternative:** Ci sono alternative come Joda-Time se le API di Java 8 non risolvono i tuoi problemi. Anche se per la maggior parte dei casi di uso, le API temporali di Java 8 dovrebbero essere sufficienti.

**3. Dettagli implementativi:** Le funzioni `plusDays()` e `minusYears()` fanno parte dell'API `java.time.LocalDate` che Kotlin utilizza per la manipolazione delle date. Modificano il valore attuale senza mutare l'oggetto originale, mantenendo l'aspetto immutabile di `LocalDate`.

## Vedi Anche:

Hai bisogno di più informazioni? Qui ci sono alcuni link utili per approfondire l'argomento:

- Documentazione ufficiale di Kotlin: [Kotlin Documentation](https://kotlinlang.org/docs/home.html)
- Documentazione ufficiale di Java 8 Date/Time API: [Java SE 8 Date and Time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Libreria Joda-Time: [Joda-Time - Java date and time API](https://www.joda.org/joda-time/)