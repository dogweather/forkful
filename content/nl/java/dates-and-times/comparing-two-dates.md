---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:28.412141-07:00
description: "Twee datums vergelijken betekent uitzoeken of de ene datum v\xF3\xF3\
  r, na, of hetzelfde is als een andere. Programmeurs doen dit om planning, deadlines,\u2026"
lastmod: '2024-03-11T00:14:24.515390-06:00'
model: gpt-4-0125-preview
summary: "Twee datums vergelijken betekent uitzoeken of de ene datum v\xF3\xF3r, na,\
  \ of hetzelfde is als een andere. Programmeurs doen dit om planning, deadlines,\u2026"
title: Twee datums vergelijken
---

{{< edit_this_page >}}

## Wat & Waarom?
Twee datums vergelijken betekent uitzoeken of de ene datum vóór, na, of hetzelfde is als een andere. Programmeurs doen dit om planning, deadlines, chronologische sortering, en meer te beheren.

## Hoe:
Java maakt het vrij eenvoudig om datums te vergelijken. Gebruik `LocalDate` en de methoden `compareTo`, `isBefore`, of `isAfter`. Hier is de kern:

```java
import java.time.LocalDate;

public class DateComparison {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2023, 4, 1);
        LocalDate date2 = LocalDate.now(); // ervan uitgaande dat vandaag 2023-4-15 is

        // Gebruik van compareTo
        int comparisonResult = date1.compareTo(date2);
        if(comparisonResult < 0) {
            System.out.println("Date1 is voor Date2");
        } else if (comparisonResult > 0) {
            System.out.println("Date1 is na Date2");
        } else {
            System.out.println("Date1 is hetzelfde als Date2");
        }

        // Gebruik van isBefore en isAfter
        if(date1.isBefore(date2)) {
            System.out.println("Date1 is eerder dan Date2");
        } else if(date1.isAfter(date2)) {
            System.out.println("Date1 is later dan Date2");
        } else {
            System.out.println("Date1 is dezelfde dag als Date2");
        }
    }
}
```

Voorbeelduitvoer voor de datum van vandaag als 2023-04-15:

```
Date1 is voor Date2
Date1 is eerder dan Date2
```

## Diepgaande duik
Historisch gezien was de datumafhandeling in Java, nou ja, een hoofdpijn. Maar toen kwam Java 8 met `java.time`, een gamechanger. Nu gebruiken we `LocalDate` voor datums zonder tijd. Wil je datums inclusief tijd vergelijken? Kijk dan naar `LocalDateTime`.

Alternatieven? Zeker. Voor Java 8 waren er `java.util.Date` en `java.util.Calendar`. Je zou ze nog steeds kunnen gebruiken, maar waarom zou je je eigen graf graven?

Wat implementatie betreft, `compareTo` retourneert een `int`: negatief als het aanroepende object minder is (voor), nul als gelijk, positief als groter (na). `isBefore` en `isAfter` retourneren een `boolean`. Makkelijk te begrijpen, zonder addertjes onder het gras.

## Zie ook
Duik voor meer details in deze bronnen:

- [Oracle's Java-documentatie over LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Oracle's tutorial over datum en tijd](https://docs.oracle.com/javase/tutorial/datetime/)
- Stack Overflow voor praktisch gebruik en probleemoplossing:
  - [Gebruik van `LocalDate`](https://stackoverflow.com/questions/tagged/localdate)
  - [Java Date versus Calendar](https://stackoverflow.com/questions/5369682/get-current-time-and-date-on-android)
