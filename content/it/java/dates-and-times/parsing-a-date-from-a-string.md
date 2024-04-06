---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:22.826195-07:00
description: ''
lastmod: '2024-04-05T21:59:55.458947-06:00'
model: gpt-4-0125-preview
summary: ''
title: Analisi di una data da una stringa
weight: 30
---

## Come fare:


### Utilizzando il pacchetto `java.time` (Raccomandato in Java 8 e successivi):
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Output: 2023-04-30
    }
}
```

### Utilizzando `SimpleDateFormat` (Approccio più vecchio):
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "30/04/2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = formatter.parse(dateString);
            System.out.println(date); // Il formato dell'output dipende dal formato di default del tuo sistema
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### Utilizzando librerie di terze parti (ad es., Joda-Time):
Joda-Time è stata una significativa libreria di terze parti ma ora è in modalità di manutenzione a causa dell'introduzione del pacchetto `java.time` in Java 8. Tuttavia, per coloro che usano versioni di Java precedenti all'8, Joda-Time è una buona scelta.
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Output: 2023-04-30
    }
}
```
Nota che quando si lavora con le date, essere sempre consapevoli delle impostazioni del fuso orario se si analizzano o si formattano date e orari piuttosto che solo date.
