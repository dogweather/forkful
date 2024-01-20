---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

In programmazione, analizzare una data da una stringa significa estrarre le informazioni di data e ora da una rappresentazione di testo. I programmatori lo fanno per manipolare o comparare date, o per formattarle per l'output.

## Come Fai:

Ecco un esempio di come fare. Usiamo `java.time`:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main(String[] args) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("d/MM/yyyy");
        LocalDate date = LocalDate.parse("23/07/2023", formatter);
        System.out.println(date); 
    }
}
```

Quando si esegue, si stamperà:

```Java
2023-07-23
```

## Approfondimenti

C'è più di quanto sembri nella formattazione di date. 

1) Storicamente, l'amministrazione delle date in Java era laboriosa. Con l'introduzione di `java.time` in Java 8, è diventato più semplice.

2) Esistono alternative. `java.text.SimpleDateFormat` era spesso usato, ma aveva problemi. Ad esempio, non era thread-safe. 

3) Gli oggetti `java.time.format.DateTimeFormatter` sono immutabili e thread-safe. Questo è importante per le applicazioni multi-thread, dove la sicurezza dei thread è una preoccupazione.

## Vedere Anche 

Per ulteriori informazioni, consultare queste altre risorse:

- La [documentazione di Oracle](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html) sul `DateTimeFormatter`.
- Questo [tutorial su Jenkov](http://tutorials.jenkov.com/java-date-time/parsing-formatting-dates.html) sulla formattazione e l'analisi delle date.
- Questo [articolo di Baeldung](https://www.baeldung.com/java-8-date-time-intro) sull'introduzione di `java.time`.