---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:36:51.341190-07:00
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Päivämäärän jäsentäminen merkkijonosta tarkoittaa sen muuttamista ohjelmoinnissa käsiteltävään muotoon. Tämä on hyödyllistä, sillä se mahdollistaa päivämäärän vertaamisen, laskennan ja muotoilun ohjelmissa.

## How to: (Kuinka tehdään:)
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-05";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        try {
            LocalDate date = LocalDate.parse(dateString, formatter);
            System.out.println("Parsed date is: " + date);
        } catch (DateTimeParseException e) {
            System.out.println("Error parsing date: " + e.getMessage());
        }
    }
}
```
Tulostus: `Parsed date is: 2023-04-05`

## Deep Dive (Syväsukellus)
Aikaisemmin Java-ohjelmoijat käyttivät `SimpleDateFormat`-luokkaa päivämäärän jäsentämiseen, mutta se oli ongelmallinen monisäikeisissä ympäristöissä. Java 8 toi `DateTimeFormatter`:n, mikä on turvallisempi ja tehokkaampi. Vaihtoehtoja on muitakin, kuten Joda-Time-kirjasto, mutta se on nyt suurelta osin korvattu `java.time`-paketin myötä. Jäsentäessä tärkeää on määritellä oikea formaatti, jotta muunnos merkkijonosta päivämäärä-olioksi onnistuu.

## See Also (Katso Myös)
- [DateTimeFormatter documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Java 8 Date and Time guide](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- [Joda-Time library](https://www.joda.org/joda-time/)
