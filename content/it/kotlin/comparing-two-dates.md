---
title:    "Kotlin: Confrontare due date"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché comparare due date in Kotlin?

Comparare due date può sembrare un'operazione semplice e banale, ma in realtà può essere molto utile in tante situazioni. Ad esempio, può essere utilizzato per verificare se un evento è già accaduto o se deve ancora accadere, per calcolare la durata di un periodo di tempo o per effettuare operazioni di ordinamento.

## Come fare la comparazione di date in Kotlin

Per confrontare due date in Kotlin, possiamo utilizzare la classe `LocalDate` del pacchetto `java.time` che ci permette di rappresentare una data senza tenere conto dell'orario o della zona temporale.

Di seguito un esempio di codice che confronta due date:

```
Kotlin
val date1 = LocalDate.of(2021, 10, 31)
val date2 = LocalDate.of(2021, 11, 13)

if (date1.isBefore(date2)) {
    println("$date1 è prima di $date2")
} else {
    println("$date1 è dopo di $date2")
}
```

L'output di questo codice sarà:

```
Kotlin
2021-10-31 è prima di 2021-11-13
```

Per ottenere un output diverso in base alle date, possiamo utilizzare i metodi `isAfter()` o `isEqual()`. Possiamo anche confrontare una data con la data attuale utilizzando il metodo `now()`:

```
Kotlin
val date = LocalDate.of(2022, 1, 1)

if (date.isBefore(LocalDate.now())) {
    println("$date è già passata")
} else {
    println("$date deve ancora arrivare")
}
```

L'output di questo codice sarà:

```
Kotlin
2022-01-01 deve ancora arrivare
```

## Approfondimento sulla comparazione di date in Kotlin

Per effettuare una comparazione più precisa, possiamo anche utilizzare la classe `LocalDateTime` che rappresenta una data e un'ora. In questo modo possiamo confrontare due date e orari completi invece che solo le date.

Un altro aspetto importante è tenere in considerazione la zona temporale nella quale viene effettuata la comparazione. Possiamo utilizzare la classe `ZonedDateTime` per rappresentare una data e un'ora nella zona temporale desiderata.

Per ulteriori informazioni e dettagli sulla gestione delle date in Kotlin, è possibile consultare la documentazione ufficiale del linguaggio.

## Vedi anche
- [Documentazione ufficiale di Kotlin](https://kotlinlang.org/docs/home.html)
- [Comparazione di date in Java](https://www.baeldung.com/java-compare-dates)
- [Utilizzo delle classi di data e ora in Kotlin](https://www.baeldung.com/kotlin-date-time)