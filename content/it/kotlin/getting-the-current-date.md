---
title:    "Kotlin: Ottenere la data corrente"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Perché

Ogni volta che sviluppiamo un'applicazione, una delle informazioni più importanti che dobbiamo avere è la data corrente. Questo ci consente di tenere traccia dei record, di effettuare calcoli e di fornire una migliore esperienza utente. In questo post, vedremo come ottenere la data corrente utilizzando Kotlin.

## Come

Per ottenere la data corrente in Kotlin, possiamo utilizzare la classe `LocalDate` del package `java.time`. La classe `LocalDate` rappresenta una data nel calendario gregoriano, senza tempo e fuso orario. Vediamo un esempio di codice di come possiamo utilizzarla:

```
Kotlin
val currentDate = LocalDate.now()
val formattedDate = currentDate.format(DateTimeFormatter.ISO_DATE)
println(formattedDate)
```

Nel codice sopra, abbiamo prima dichiarato una variabile `currentDate` che ci permette di ottenere la data corrente utilizzando il metodo `now()` della classe `LocalDate`. Successivamente, abbiamo utilizzato la classe `DateTimeFormatter` per formattare la data secondo lo standard ISO_DATE e l'abbiamo assegnata alla variabile `formattedDate`. Infine, abbiamo stampato la data formattata utilizzando il metodo `println()`.

Se eseguiamo questo codice, l'output sarebbe qualcosa del genere:

`2019-09-24`

Oltre a questo, possiamo anche ottenere altre informazioni sulla data corrente utilizzando i metodi della classe `LocalDate`. Ad esempio, possiamo ottenere il giorno della settimana, il giorno del mese, il mese e l'anno. Vediamo un esempio di come possiamo utilizzare questi metodi:

```
Kotlin
val currentDate = LocalDate.now()
println(currentDate.dayOfWeek) // stampa il giorno della settimana
println(currentDate.dayOfMonth) // stampa il giorno del mese
println(currentDate.month) // stampa il mese
println(currentDate.year) // stampa l'anno
```

L'output sarebbe qualcosa del genere:

```
TUESDAY
24
SEPTEMBER
2019
```

## Deep Dive

In questo post abbiamo usato la classe `LocalDate` per ottenere la data corrente. Questa classe ha molti altri metodi utili che possiamo utilizzare per manipolare e gestire le date in modo efficace. Ad esempio, possiamo utilizzare il metodo `isLeapYear()` per verificare se un determinato anno è bisestile o il metodo `plusDays()` per aggiungere un numero specifico di giorni alla data corrente.

È importante notare che la classe `LocalDate` è thread-safe, il che significa che possiamo utilizzarla in applicazioni multithread senza rischi di side-effects.

## Vedi anche

- [Documentazione ufficiale di Java SE 11 per la classe LocalDate](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
- [Tutorial di Kotlin per principianti](https://kotlinlang.org/docs/tutorials/getting-started.html)
- [Documentazione ufficiale di Kotlin](https://kotlinlang.org/docs/reference/)

Scritto con ♥️ da <Your Name>