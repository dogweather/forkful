---
title:                "Java: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere un'operazione utile in diversi contesti, come per la pianificazione di eventi o la gestione di scadenze.

## Come Fare

Per calcolare una data nel futuro o nel passato, è possibile utilizzare la classe `LocalDate` del package `java.time`. Questa classe consente di gestire le date e le operazioni su di esse in modo semplice ed efficiente. Di seguito un esempio di codice per calcolare una data nel futuro:

```Java
import java.time.LocalDate;

LocalDate dataOdierna = LocalDate.now(); //Data odierna
LocalDate dataFutura = dataOdierna.plusDays(10); //Data fra 10 giorni
System.out.println("La data fra 10 giorni sarà: " + dataFutura);
```

L'output di questo codice sarà: `La data fra 10 giorni sarà: 2021-03-30`. È possibile anche calcolare una data nel passato utilizzando il metodo `minusDays()` al posto di `plusDays()`.

## Approfondimento

Per calcolare una data nel futuro o nel passato, è importante tenere in considerazione alcuni elementi. In primo luogo, è fondamentale selezionare il formato della data corretto, ad esempio utilizzando `dd-MM-yyyy` per una formattazione giorno-mese-anno. Inoltre, è possibile effettuare operazioni su date specifiche, come ad esempio verificare se un anno è bisestile utilizzando il metodo `isLeapYear()`.

## Vedi Anche
- [Documentazione ufficiale di Java su LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Esempi di utilizzo di LocalDate](https://www.baeldung.com/java-date-in-the-past-future)
- [Come gestire le date e le ore in Java](https://dzone.com/articles/how-to-handle-date-time-number-formats-and-locales)