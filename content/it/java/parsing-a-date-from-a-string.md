---
title:                "Analizzare una data da una stringa"
html_title:           "Java: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Il parsing di una data da una stringa è il processo di trasformare una data espressa come stringa di testo in un oggetto di tipo data all'interno di un programma Java. I programmatori fanno questo per elaborare e manipolare le date in modo più efficace all'interno del loro codice.

## Come fare:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

//Creiamo una stringa con una data
String dataStringa = "01/01/2021";

try {
    //Definiamo il formato della data da utilizzare
    SimpleDateFormat formatoData = new SimpleDateFormat("dd/MM/yyyy");
    //Trasformiamo la stringa in un oggetto data
    Date data = formatoData.parse(dataStringa);
    //Stampiamo l'oggetto data
    System.out.println(data);
} catch (java.text.ParseException e) {
    //In caso di errore nella conversione, stampiamo un messaggio di errore
    System.out.println("Errore nella conversione della data.");
}
```
Output:
```
Fri Jan 01 00:00:00 CET 2021
```
## Approfondimento:

Il parsing di una data da una stringa è diventato un'operazione comune dopo l'introduzione della libreria Joda-Time nel 2002. Questa libreria ha portato numerosi miglioramenti alle operazioni di gestione delle date in Java ed è stata in seguito integrata nel pacchetto di base di Java a partire dalla versione 8. Oltre all'utilizzo della classe SimpleDateFormat come mostrato nell'esempio precedente, è possibile utilizzare anche la classe DateTimeFormatter per effettuare il parsing di una data da una stringa.

## Vedi anche:

- [Java SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Joda-Time](https://www.joda.org/joda-time/)