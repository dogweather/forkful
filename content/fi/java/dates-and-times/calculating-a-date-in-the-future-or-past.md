---
date: 2024-01-20 17:31:17.132933-07:00
description: "N\xE4in se tehd\xE4\xE4n: Java tarjoaa `LocalDate`-luokan p\xE4iv\xE4\
  m\xE4\xE4rien k\xE4sittelyyn. T\xE4ss\xE4 on esimerkki, jossa lasketaan tulevaisuuden\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4."
lastmod: '2024-03-13T22:44:56.458593-06:00'
model: gpt-4-1106-preview
summary: "Java tarjoaa `LocalDate`-luokan p\xE4iv\xE4m\xE4\xE4rien k\xE4sittelyyn."
title: "Tulevan tai menneen p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

## Näin se tehdään:
Java tarjoaa `LocalDate`-luokan päivämäärien käsittelyyn. Tässä on esimerkki, jossa lasketaan tulevaisuuden päivämäärä:

```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class DateCalculator {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        LocalDate tenDaysLater = today.plusDays(10);

        System.out.println("Tänään: " + today);
        System.out.println("Kymmenen päivän päästä: " + tenDaysLater);
        
        LocalDate sixMonthsAgo = today.minus(6, ChronoUnit.MONTHS);
        System.out.println("Kuusi kuukautta sitten: " + sixMonthsAgo);
    }
}
```

Tulostus:
```
Tänään: 2023-04-01
Kymmenen päivän päästä: 2023-04-11
Kuusi kuukautta sitten: 2022-10-01
```

## Syväsukellus
Aikaisemmin Java käytti `Date`- ja `Calendar`-luokkia, mutta ne olivat kömpelöitä ja virhealttiita. Java 8 toi mukanaan `java.time`-paketin, joka on vahvempi ja selkeämpi.

Vaihtoehtoja sisäänrakennetuille työkaluille ovat Joda-Time-kirjasto tai kolmansien osapuolien date and time -kirjastot. Joda-Time innoitti `java.time`-paketin, mutta sen jälkeen se on suositeltu korvattavaksi Javan uudemmilla työkaluilla.

Päivämäärien laskemisessa voi esiintyä aikavyöhykeongelmia. `ZonedDateTime` auttaa hallitsemaan aikavyöhykkeiden eroja. Tämä on tärkeää, kun sovellukset toimivat globaalisti eri aikavyöhykkeillä.

## Katso myös
- Java 8 Date and Time guide: https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html
- Joda-Time kotisivu: https://www.joda.org/joda-time/
- Tutorial Java `java.time`: https://www.baeldung.com/java-8-date-time-intro
