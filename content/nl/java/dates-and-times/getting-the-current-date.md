---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:14.737043-07:00
description: ''
lastmod: '2024-04-05T22:51:03.523472-06:00'
model: gpt-4-0125-preview
summary: ''
title: Het huidige datum ophalen
weight: 29
---

## Hoe:


### De Datum van Vandaag Ophalen
```java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {
        LocalDate vandaag = LocalDate.now();
        System.out.println("Datum van vandaag: " + vandaag);
    }
}
```

**Voorbeelduitvoer:**
```
Datum van vandaag: 2023-04-01
```

### Tijd met Meer Details
```java
import java.time.LocalDateTime;

public class Main {
    public static void main(String[] args) {
        LocalDateTime nu = LocalDateTime.now();
        System.out.println("Huidige Datum en Tijd: " + nu);
    }
}
```

**Voorbeelduitvoer:**
```
Huidige Datum en Tijd: 2023-04-01T12:45:30.123
```

## Diepere Duik:
Voor Java 8 werden `java.util.Date` en `java.util.Calendar` gebruikt voor datum-tijd. Maar deze waren omslachtig en niet intuïtief. Java 8 introduceerde `java.time`, een robuustere en begrijpelijker API. `java.time.LocalDate` haalt de datum zonder tijd, terwijl `java.time.LocalDateTime` datum en tijd haalt, zonder een tijdzone. Als je de tijdzone nodig hebt, is er `java.time.ZonedDateTime`. Voor alleen tijd, is er `java.time.LocalTime`.

Wat betreft alternatieven, bibliotheken zoals Joda-Time bestonden voor Java 8, en sommige oude projecten gebruiken het misschien nog steeds. Maar sinds de introductie van het `java.time` pakket in Java 8, wordt het beschouwd als de standaard, en om goede redenen. Het is uitgebreid en in lijn met het ISO-8601 kalendersysteem.

Vanuit implementatieperspectief halen `now()` methoden binnen `java.time` klassen de huidige datum/tijd uit de systeemklok, wat de computeropvatting van huidige tijd is, verbonden met de echte wereld door systeeminstellingen en internet tijdsynchronisatie.

## Zie Ook:
- De officiële `java.time` pakketdocumentatie: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- ISO 8601 Datum en Tijd Standaarden: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)
- Voor ouderwets Java datumbeheer, bekijk de `Calendar` klasse: [https://docs.oracle.com/javase/7/docs/api/java/util/Calendar.html](https://docs.oracle.com/javase/7/docs/api/java/util/Calendar.html)
