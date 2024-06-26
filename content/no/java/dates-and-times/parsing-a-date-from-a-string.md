---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:28.543908-07:00
description: ''
lastmod: '2024-04-05T21:59:53.180597-06:00'
model: gpt-4-0125-preview
summary: ''
title: Analysering av en dato fra en streng
weight: 30
---

## Hvordan:


### Bruker `java.time` pakken (Anbefalt i Java 8 og senere):
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Utdata: 2023-04-30
    }
}
```

### Bruker `SimpleDateFormat` (Eldre tilnærming):
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
            System.out.println(date); // Utdataformat avhenger av systemets standardformat
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### Bruker tredjepartsbiblioteker (f.eks., Joda-Time):
Joda-Time har vært et betydelig tredjepartsbibliotek, men er nå i vedlikeholdsmodus på grunn av introduksjonen av `java.time` pakken i Java 8. Imidlertid, for de som bruker Java-versjoner før 8, er Joda-Time et godt valg.
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Utdata: 2023-04-30
    }
}
```
Merk at når man arbeider med datoer, bør man alltid være oppmerksom på tidssoneinnstillingene hvis man analyserer eller formaterer datotider i stedet for bare datoer.
