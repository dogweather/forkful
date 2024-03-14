---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:29.595722-07:00
description: "Att tolka ett datum fr\xE5n en str\xE4ng inneb\xE4r att konvertera textrepresentationen\
  \ av ett datum och tid till ett `Date`-objekt eller ett modernare\u2026"
lastmod: '2024-03-13T22:44:37.796436-06:00'
model: gpt-4-0125-preview
summary: "Att tolka ett datum fr\xE5n en str\xE4ng inneb\xE4r att konvertera textrepresentationen\
  \ av ett datum och tid till ett `Date`-objekt eller ett modernare\u2026"
title: "Analysera ett datum fr\xE5n en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng innebär att konvertera textrepresentationen av ett datum och tid till ett `Date`-objekt eller ett modernare `LocalDateTime`-objekt. Programmerare gör detta för att manipulera, formatera, jämföra eller lagra datum i ett standardiserat format, vilket är avgörande för applikationer som kräver datumberäkningar, validering eller konsekvent internationalisering.

## Hur man gör:

### Använda `java.time`-paketet (Rekommenderas i Java 8 och senare):
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Utmatning: 2023-04-30
    }
}
```

### Använda `SimpleDateFormat` (Äldre tillvägagångssätt):
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
            System.out.println(date); // Utmatningsformat beror på ditt systems standardformat
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### Använda tredjepartbibliotek (t.ex. Joda-Time):
Joda-Time har varit ett betydande tredjepartbibliotek men är nu i underhållsläge på grund av introduktionen av `java.time`-paketet i Java 8. För de som använder Java-versioner före 8 är dock Joda-Time ett bra val.
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Utmatning: 2023-04-30
    }
}
```
Notera att när man arbetar med datum, var alltid medveten om tidszoninställningarna om du tolkar eller formaterar datum-tider istället för bara datum.
