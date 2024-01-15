---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "Java: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Koodin kirjoittaminen on usein hyvin spesifistä. Jokaisen ohjelmoijan täytyy joskus muuttaa kalenteripäivämäärä merkkijonoksi ja tämä on tärkeä taito, joka lisää joustavuutta ja ymmärrystä ohjelmoinnissa.

## Kuinka

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateToString {

    public static void main(String[] args) {
        
        // Luodaan LocalDate-olio nykyisestä päivämäärästä
        LocalDate today = LocalDate.now();

        // Määritetään haluttu muoto
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");

        // Muutetaan LocalDate merkkijonoksi käyttäen valittua muotoa
        String dateAsString = today.format(formatter);

        // Tulostetaan tulos
        System.out.println("Päivämäärä merkkijonona: " + dateAsString);
    }
}
```

**Tulos:**

```
Päivämäärä merkkijonona: 21.09.2021
```

## Syväsukellus

Java-kielen LocalDate-luokka tarjoaa useita eri metodeja päivämääräolion muuttamiseksi merkkijonoksi. Yllä olevassa esimerkissä käytimme **format()**-metodia, joka ottaa vastaan **DateTimeFormatter**-olion ja palauttaa lopputuloksen merkkijonona. Voit myös käyttää **toString()**-metodia, joka palauttaa päivämäärän oletusmuodossa tai **format(DateTimeFormatter.BASIC_ISO_DATE)**-metodia, joka palauttaa päivämäärän ISO-standardin mukaisessa muodossa.

On tärkeää muistaa, että kaikissa näissä metodeissa käytetty **DateTimeFormatter**-olio määrää, millaiseen muotoon päivämäärä muutetaan. Voit esimerkiksi käyttää **DateTimeFormatter.ofPattern()**-metodia ja määrittää haluamasi muodon käyttämällä erilaisia merkkejä, kuten "MM" kuukaudelle tai "yyyy" vuodelle.

## Katso myös

- [Java Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java String-luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)