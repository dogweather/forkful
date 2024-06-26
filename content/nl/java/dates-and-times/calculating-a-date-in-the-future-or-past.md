---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:32.072862-07:00
description: 'Hoe: De output kan er als volgt uitzien.'
lastmod: '2024-04-05T21:53:50.719541-06:00'
model: gpt-4-0125-preview
summary: De output kan er als volgt uitzien.
title: Een datum in de toekomst of het verleden berekenen
weight: 26
---

## Hoe:
```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class DateCalculatie {
    public static void main(String[] args) {
        LocalDate vandaag = LocalDate.now();
        // Voeg 10 dagen toe aan de huidige datum
        LocalDate toekomstDate = vandaag.plusDays(10);
        System.out.println("Toekomstige Datum: " + toekomstDate);

        // Trek 2 maanden af van de huidige datum
        LocalDate verledenDatum = vandaag.minus(2, ChronoUnit.MONTHS);
        System.out.println("Verleden Datum: " + verledenDatum);
    }
}
```

De output kan er als volgt uitzien:

```
Toekomstige Datum: 2023-04-30
Verleden Datum: 2023-02-20
```

## Diep Duiken
Voor Java 8 was het manipuleren van datums een pijn. Oude klassen zoals `java.util.Date` en `java.util.Calendar` waren gevoelig voor bugs en niet gebruiksvriendelijk. Het `java.time` pakket geïntroduceerd in Java 8 loste dit op met doordachte klassen zoals `LocalDate`, `LocalTime`, en `ZonedDateTime`.

Alternatieven? In het pre-Java 8 tijdperk waren externe bibliotheken zoals Joda-Time gangbaar. Tegenwoordig kun je ze nog steeds gebruiken, maar het standaard `java.time` wordt aanbevolen omdat het officieel deel uitmaakt van Java en zaken zoals zomertijd, tijdzones en schrikkeljaren elegant afhandelt.

Bij het coderen van datum berekeningen, overweeg tijdzones als je context dit vereist. Voor UTC, gebruik `Instant` in plaats van `LocalDate`. Voor specifieke zones zou je typisch `ZonedDateTime` gebruiken. Onthoud, datum-tijd operaties kunnen worden geketend, zoals `datum.minWeeken(1).plusUren(3)`, wat je code schoner maakt.

## Zie Ook
1. Het `java.time` pakketoverzicht: [Oracle Docs](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
2. Tijdzone afhandeling met `ZonedDateTime`: [Oracle ZonedDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/ZonedDateTime.html)
3. Officiële datum- en tijdpatronen voor `java.time.format.DateTimeFormatter`: [Oracle DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
