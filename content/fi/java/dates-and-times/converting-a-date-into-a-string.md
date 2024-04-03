---
date: 2024-01-20 17:36:38.613492-07:00
description: "How to: (Kuinka tehd\xE4:) ."
lastmod: '2024-03-13T22:44:56.456752-06:00'
model: gpt-4-1106-preview
summary: .
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

## How to: (Kuinka tehdä:)
```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateToStringExample {
    public static void main(String[] args) {
        LocalDate date = LocalDate.now(); // Nykyinen päivämäärä
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy"); // Suomalainen formaatti
        String textDate = date.format(formatter); // Päivämäärän muunto merkkijonoksi

        System.out.println(textDate); // Tulostaa esim. "24.03.2023"
    }
}
```

## Deep Dive (Sukellus syvemmälle)
Aiemmin Java käytti `SimpleDateFormat` luokkaa, mutta se tuli monimutkaiseksi ja se ei ollut säieturvallinen. Java 8 toi paikalle `DateTimeFormatter`in, joka korjasi nämä ongelmat ja muut. Vaihtoehtoina voi käyttää kolmannen osapuolen kirjastoja kuten Joda-Time, mutta Java 8:n jälkeen tämä on harvemmin tarpeen. `DateTimeFormatter`in avulla voit määrittää päivämäärän esitystavan laajasti, ja se huomioi myös lokalisoinnit.

## See Also (Katso myös):
- [DateTimeFormatter dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [LocalDate dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Date and Time API opas](https://www.baeldung.com/java-8-date-time-intro)
