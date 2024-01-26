---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:15:22.105378-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Ottenere la data corrente in Java significa riuscire a leggere il momento preciso in cui il programma è in esecuzione. I programmatori lo fanno per registrare eventi, misurare intervalli temporali, o semplicemente mostrare date e orari agli utenti.

## How to: (Come fare:)
```java
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class GetDateExample {
    public static void main(String[] args) {
        // Ottiene la data corrente
        LocalDate today = LocalDate.now();
        System.out.println("Data di oggi: " + today);
        
        // Ottiene la data e ora correnti
        LocalDateTime dateTimeNow = LocalDateTime.now();
        System.out.println("Data e ora attuali: " + dateTimeNow);
        
        // Formatta la data e l'ora in un modo leggibile
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss");
        String formattedDateTime = dateTimeNow.format(formatter);
        System.out.println("Data e ora formattati: " + formattedDateTime);
    }
}
```
Output:
```
Data di oggi: 2023-04-04
Data e ora attuali: 2023-04-04T15:20:55.123456
Data e ora formattati: 04/04/2023 15:20:55
```

## Deep Dive (Approfondimento)
Java ha avuto diverse API per gestire date e orari nel corso degli anni. `java.util.Date` era l'inizio, ma aveva problemi di design e mancanza di supporto per fusi orari. `java.util.Calendar` ha tentato di risolvere alcuni di quei problemi, ma non era ancora l'ideale. Dal Java 8 in poi, possiamo usare l'API `java.time`, più robusta e intuitiva, basata sul progetto Joda-Time. 

Per esempio, `LocalDate` e `LocalDateTime` sono classi immutabili e thread-safe dell'API `java.time`, che ci danno rispettivamente solo la data o la data con l'ora. Queste classi hanno anche metodi per l'addizione e la sottrazione di giorni, settimane e altri unità temporali. 

Un'altra parte importante è il formattatore, `DateTimeFormatter`, che ci permette di convertire le date e gli orari in stringhe leggibili o di interpretare date e orari formattati come stringhe.

Rispetto alle vecchie API, `java.time` è più coerente e affidabile, rendendo il lavoro con date e orari molto più gestibile.

## See Also (Vedi Anche)
- [LocalDate Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [LocalDateTime Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [DateTimeFormatter Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Project Joda-Time](https://www.joda.org/joda-time/)
- [Tutorial Oracle sulla gestione di data e ora](https://docs.oracle.com/javase/tutorial/datetime/)
