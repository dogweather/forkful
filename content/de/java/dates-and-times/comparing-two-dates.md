---
date: 2024-01-20 17:33:15.967202-07:00
description: "So geht's: Datumvergleiche in Java haben eine lange Historie. Urspr\xFC\
  nglich nutzte man `java.util.Date` und `java.util.Calendar`, aber beide hatten ihre\u2026"
lastmod: '2024-04-05T21:53:55.658165-06:00'
model: gpt-4-1106-preview
summary: Datumvergleiche in Java haben eine lange Historie.
title: Vergleich von zwei Daten
weight: 27
---

## So geht's:
```java
import java.time.LocalDate;
import java.time.Month;

public class DateComparison {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2023, Month.MARCH, 10);
        LocalDate date2 = LocalDate.now();
        
        System.out.println("Das erste Datum: " + date1);
        System.out.println("Das zweite Datum: " + date2);

        if (date1.isAfter(date2)) {
            System.out.println("Das erste Datum liegt nach dem zweiten Datum.");
        } else if (date1.isBefore(date2)) {
            System.out.println("Das erste Datum liegt vor dem zweiten Datum.");
        } else {
            System.out.println("Die Daten sind gleich.");
        }
    }
}
/*
Ausgabe kann variieren, da date2 das aktuelle Datum beim Ausführen verwendet:
Das erste Datum: 2023-03-10
Das zweite Datum: [aktuelles Datum]
Das erste Datum liegt vor dem zweiten Datum.
*/
```

## Deep Dive:
Datumvergleiche in Java haben eine lange Historie. Ursprünglich nutzte man `java.util.Date` und `java.util.Calendar`, aber beide hatten ihre Tücken in der Bedienung. Mit Java 8 kam `java.time`, die modernere und robustere API. Alternativen außerhalb der Standardbibliotheken sind Joda-Time und die Apache Commons Lang Time Utilities. Für komplexe Vergleiche kann man `.isEqual()`, `.isAfter()`, `.isBefore()` benutzen oder sogar Comparator-Logik mit `.compareTo()` für Listen von Datumsobjekten.

## Siehe auch:
- [Oracle Java Docs – LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Apache Commons Lang](https://commons.apache.org/proper/commons-lang/)
- [Joda-Time Library](https://www.joda.org/joda-time/)
