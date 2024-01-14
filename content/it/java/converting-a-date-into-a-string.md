---
title:                "Java: Convertingire una data in una stringa"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una data in una stringa può essere utile per visualizzare o stampare la data in un formato specifico, ad esempio per mostrare una data sul front-end di un'applicazione.

## Come fare

La conversione di una data in una stringa può essere effettuata utilizzando la classe SimpleDateFormat di Java. Ecco un esempio di codice che converte una data in una stringa nel formato "dd/MM/YYYY":

```Java
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/YYYY");
Date date = new Date();
String dateString = sdf.format(date);

System.out.println(dateString);
```

L'output di questo codice sarà: "04/10/2021", dove "04" rappresenta il giorno, "10" il mese e "2021" l'anno corrente.

Inoltre, è possibile utilizzare altre opzioni di formattazione per ottenere una stringa con il nome del mese o il giorno della settimana. Ad esempio:

```Java
SimpleDateFormat sdf = new SimpleDateFormat("dd MMMM, YYYY");
Date date = new Date();
String dateString = sdf.format(date);

System.out.println(dateString);
```

L'output di questo codice sarà: "04 ottobre, 2021", dove "ottobre" è il nome del mese in italiano.

## Approfondimento

La classe SimpleDateFormat ha diverse costanti che possono essere utilizzate per personalizzare ulteriormente il formato della data. Ad esempio, è possibile aggiungere l'ora e i minuti utilizzando "HH:mm" come pattern di formato.

Inoltre, la classe Date di Java è stata deprecata a partire dalla versione 8 e consigliata l'utilizzo della classe LocalDate per rappresentare una data. Per convertire LocalDate in una stringa, è possibile utilizzare il metodo format della classe DateTimeFormatter.

## Vedi anche

- [Documentazione ufficiale di Java per SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Guida su come utilizzare la classe LocalDate in Java](https://www.baeldung.com/java-8-date-time-intro)
- [Tutorial su come utilizzare la classe DateTimeFormatter per convertire LocalDate in una stringa](https://www.baeldung.com/java-datetimeformatter)