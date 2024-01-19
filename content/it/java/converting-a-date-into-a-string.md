---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Convertire Una Data in Una Stringa in Java: Una Guida Semplificata  

## Cos'è e perché?
Convertire una data in una stringa significa rappresentare un oggetto di tipo Date, con informazioni come giorno, mese, anno, ora, minuti, e secondi, in un formato testuale. I programmatori lo fanno per la leggibilità e la facilità di manipolazione, specialmente quando si tratta di registrazioni di log, output utente e memorizzazione dei dati.

## Come si fa:
```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
    public static void main(String[] args) {
        Date now = new Date();
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");

        String strDate = formatter.format(now);
        System.out.println("La data e l'ora correnti in formato stringa: " + strDate);
    }
}
```
Una possibile uscita potrebbe essere:
```   
La data e l'ora correnti in formato stringa: 25-09-2021 10:31:58
```

## Approfondimento
La classe SimpleDateFormat, introdotta nel JDK 1.1, fornisce metodi per il parsing e la formattazione di date. Però, a causa delle sue carenze in termini di thread-safety e design, nel JDK 8 è stata introdotta una nuova API per la data e l'ora. Per convertire la data in stringa potresti usare il metodo `format()` della classe `DateTimeFormatter`:

```Java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main(String[] args) {
        LocalDateTime now = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss");

        String strDate = now.format(formatter);
        System.out.println("La data e l'ora correnti in formato stringa: " + strDate);
    }
}
```
L'uscita sarà la stessa dell'esempio precedente.

## Per Saperne Di Più
Per ulteriori dettagli, consultare le seguenti risorse:

1. Tutorial Oracle su Date e Time: https://docs.oracle.com/javase/tutorial/datetime/
2. Java SimpleDateFormat - Javatpoint: https://www.javatpoint.com/java-simpledateformat
3. DateTimeFormatter (Java SE 11 & JDK 11): https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/format/DateTimeFormatter.html
4. Come convertire date in stringhe e viceversa in Java - StackOverflow: https://stackoverflow.com/questions/5683728/convert-java-util-date-to-string