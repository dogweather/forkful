---
title:                "Java: Päivämäärän hakeminen"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Monet Java-ohjelmoijat tarvitsevat työssään nykyisen päivämäärän tietoa erilaisiin tarkoituksiin. Tämä voi olla esimerkiksi päivämäärän näyttäminen käyttäjälle tai jonkin tapahtuman ajastaminen tiettyyn päivämäärään. Joten, kuinka voimme saada nykyisen päivämäärän Java-ohjelmassa?

## Kuinka

Päivämäärän saamiseksi Java-ohjelmassa tarvitaan `java.time` -pakettia. Tämä paketti sisältää `LocalDate` -luokan, jota voidaan käyttää nykyisen päivämäärän luomiseen. Tässä on yksinkertainen esimerkki:

```java
import java.time.LocalDate;

public class CurrentDateExample {

    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate);
    }
}
```

Tämän koodin tuloste on nykyinen päivämäärä muodossa `VVVV-KK-PP`, esimerkiksi `2021-08-11`.

`LocalDate.now()` -metodi palauttaa nykyisen päivämäärän. Voimme myös käyttää muita metodeja, kuten `getDayOfMonth()` ja `getMonthValue()` saadaksemme tarkempaa tietoa päivämäärästä.

```java
import java.time.LocalDate;

public class CurrentDateExample {

    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println("Päivä: " + currentDate.getDayOfMonth());
        System.out.println("Kuukausi: " + currentDate.getMonthValue());
    }
}
```

Tuloste:

```
Päivä: 11
Kuukausi: 8
```

## Syventävä sukellus

Nykyisen päivämäärän saaminen ei rajoitu vain `LocalDate` -luokkaan. Voimme myös käyttää muita `java.time` -paketin luokkia, kuten `LocalDateTime` ja `ZonedDateTime` saadaksemme tietoa päivämäärän lisäksi myös ajasta ja aikavyöhykkeestä.

Päivämäärän käsittely on tärkeä osa Java-ohjelmointia, ja `java.time` -paketti tarjoaa monia hyödyllisiä metodeja ja luokkia tähän tarkoitukseen.

## Katso myös

- [Java 8 Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java LocalDate Example](https://www.javatpoint.com/java-localdate)
- [Java Date and Time Tutorials](https://www.tutorialspoint.com/java_date_time/index.htm)