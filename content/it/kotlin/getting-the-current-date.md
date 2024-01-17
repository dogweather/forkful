---
title:                "Ottenere la data corrente"
html_title:           "Kotlin: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Ricevere la data corrente è il processo di ottenere la data e l'ora attuali su un dispositivo o sistema. I programmatori spesso devono utilizzare la data corrente per creare funzionalità utili come segnalibri temporali, timer o dati di registrazione.

## Come:
```
Kotlin val currentDate = currentDate()

Kotlin fun main() {
  println("La data corrente è: $currentDate")
}
```
Output:
```
La data corrente è: Fri Jul 09 09:20:10 UTC 2021
```

## Approfondimento:
La possibilità di ottenere la data corrente è diventata più facile con l'avvento dei linguaggi di programmazione moderni come Kotlin. In passato, i programmatori dovevano scrivere codice complesso per ottenere la data corrente, mentre ora può essere fatto con poche righe di codice. C'è una varietà di metodi per ottenere la data corrente in Kotlin, tra cui l'utilizzo della classe Java Date, l'utilizzo della libreria di terze parti Joda-Time o utilizzando la nuova classe Java 8 LocalDateTime. Tuttavia, l'utilizzo della funzione incorporata di Kotlin per ottenere la data corrente è il metodo più raccomandato.

## Vedi anche:
- [Documentazione ufficiale di Kotlin: `currentDate`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/current-date.html)
- [Tutorial di Tutorials Point su come ottenere la data corrente in Kotlin](https://www.tutorialspoint.com/how-to-get-current-date-time-in-kotlin)