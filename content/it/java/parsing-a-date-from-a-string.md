---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:36:35.745682-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
`Parsing` una data significa convertirla da `String` a un oggetto `Date`. Programmatori lo fanno per manipolare e confrontare date, o per cambiarne il formato.

## How to:
Usiamo `java.time.format.DateTimeFormatter` e `java.time.LocalDate`:

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

public class DateParser {
    public static void main(String[] args) {
        String dataStringa = "15/04/2023";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");

        try {
            LocalDate data = LocalDate.parse(dataStringa, formatter);
            System.out.println("Data convertita: " + data);
        } catch (DateTimeParseException e) {
            System.out.println("Errore nel parsing della data.");
        }
    }
}
```
Output:
```
Data convertita: 2023-04-15
```

## Deep Dive
In passato, Java usava `java.util.Date` e `SimpleDateFormat`, ma avevano problemi di thread-safety e design. Java 8 ha introdotto `java.time`, più robusto e intuitivo. Alternativamente, ci sono librerie esterne come Joda-Time, ma con `java.time` non c'è quasi più bisogno.

Nell'esempio, `DateTimeFormatter` definisce il formato della data. `LocalDate.parse()` serve per effettuare il parsing vero e proprio. Se il formato non è corretto, salta fuori un `DateTimeParseException`. `java.time` supporta anche ZoneId per date e orari con fusi orari diversi.

## See Also
- [DateTimeFormatter documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Oracle tutorial on date and time](https://docs.oracle.com/javase/tutorial/datetime/)
- [Joda-Time library](https://www.joda.org/joda-time/)
