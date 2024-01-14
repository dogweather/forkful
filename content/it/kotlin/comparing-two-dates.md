---
title:                "Kotlin: Confrontare due date."
simple_title:         "Confrontare due date."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché
Scegliere la data giusta è un aspetto fondamentale nella programmazione e può essere una sfida, soprattutto quando si devono confrontare due date. In questo articolo, scoprirai come comparare due date in Kotlin e risolvere questo problema in modo efficiente.

## Come fare
Per comparare due date in Kotlin, utilizzeremo la classe `LocalDate` del pacchetto `java.time`. Iniziamo importando il pacchetto all'inizio del nostro codice:
```Kotlin
import java.time.LocalDate
```
Dopo aver importato il pacchetto, possiamo creare due oggetti `LocalDate` da comparare:
```Kotlin
val date1 = LocalDate.of(2021, 8, 23)
val date2 = LocalDate.now()
```
In questo esempio, abbiamo creato due oggetti `LocalDate`, uno con la data del 23 agosto 2021 e l'altro con la data odierna. Ora possiamo utilizzare il metodo `isBefore()` per verificare se la prima data è precedente alla seconda:
```Kotlin
if (date1.isBefore(date2)) {
    println("La prima data è precedente alla seconda.")
} else {
    println("La prima data è successiva alla seconda.")
}
```
L'output sarà "La prima data è precedente alla seconda." poiché il 23 agosto 2021 viene prima della data odierna.

## Deep Dive
Il metodo `isBefore()` confronta solo la data e non tiene conto dell'orario. Se vuoi confrontare anche l'ora, puoi utilizzare il metodo `isBefore()` della classe `LocalDateTime`. Inoltre, se vuoi confrontare anche il fuso orario, puoi utilizzare il metodo `isBefore()` della classe `ZonedDateTime`.

Un'altra opzione per comparare due date è utilizzare il metodo `compareTo()` della classe `LocalDate`:
```Kotlin
val result = date1.compareTo(date2)
if (result < 0) {
    println("La prima data è precedente alla seconda.")
} else if (result > 0) {
    println("La prima data è successiva alla seconda.")
} else { // result == 0
    println("Le due date sono uguali.")
}
```
Il metodo `compareTo()` restituisce un numero negativo se la prima data è precedente alla seconda, un numero positivo se è successiva o 0 se sono uguali.

## Vedi anche
- [Documentazione di Kotlin](https://kotlinlang.org/docs/datetime.html)
- [Tutorial di JavaTime per principianti](https://www.baeldung.com/java-time)
- [Guida completa a Java 8 Date/Time API](https://www.baeldung.com/java-8-date-time-intro)