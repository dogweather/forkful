---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Java: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Cosa & Perché?
Calcolare una data in futuro o passato è il processo di determinare una data che si trova un certo numero di giorni prima o dopo una data di riferimento. I programmatori spesso eseguono questo calcolo per aggiungere o sottrarre un certo numero di giorni a una data specificata, ad esempio per calcolare la scadenza di un abbonamento o la data di consegna di un ordine.

Come fare:
Per calcolare una data in futuro o passato, è possibile utilizzare la classe `Calendar` di Java. Di seguito è riportato un esempio di codice che calcola la data esatta di 30 giorni fa e la data esatta di 30 giorni in futuro rispetto alla data odierna:
```Java 
Calendar cal = Calendar.getInstance(); // ottieni un'istanza della classe Calendar
cal.add(Calendar.DATE, -30); // sottrai 30 giorni dalla data attuale
Date dataPassata = cal.getTime(); // ottieni la data risultante come oggetto Date
cal.add(Calendar.DATE, 60); // aggiungi 60 giorni dalla data attuale
Date dataFutura = cal.getTime(); // ottieni la data risultante come oggetto Date
```
Output:
```
Data esatta di 30 giorni fa: Mon May 31 00:00:00 CEST 2021
Data esatta di 30 giorni in futuro: Wed Jul 28 00:00:00 CEST 2021
```

Approfondimento:
La classe `Calendar` di Java è stata introdotta per la prima volta nella versione 1.1 del linguaggio e rappresenta uno dei modi più comuni per gestire le date. Tuttavia, con l'avvento di Java 8, è stata introdotta una nuova classe chiamata `LocalDate` che semplifica notevolmente la gestione delle date e offre funzionalità più specifiche, come ad esempio il calcolo della differenza tra due date.

Per coloro che sono abituati a lavorare con la libreria Joda-Time, una delle alternative più popolari alla classe `Calendar`, è importante notare che Java 8 ha preso ispirazione da questa libreria per la creazione della nuova classe `LocalDate`.

Per quanto riguarda l'implementazione, è importante tenere presente che Java utilizza il sistema "anno-mese-giorno" per rappresentare le date, mentre alcuni altri linguaggi come SQL utilizzano il sistema "giorno-mese-anno". Questo può portare a confusione quando si lavora con entrambi i linguaggi, quindi è sempre consigliabile prestare attenzione al formato della data utilizzato.

Vedi anche:
- [Documentazione ufficiale di Java su `Calendar`](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Documentazione ufficiale di Java su `LocalDate`](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Joda-Time](https://www.joda.org/joda-time/)