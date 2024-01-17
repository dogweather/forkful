---
title:                "Hanki nykyinen päivämäärä."
html_title:           "Java: Hanki nykyinen päivämäärä."
simple_title:         "Hanki nykyinen päivämäärä."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän saaminen tarkoittaa nykyisen päivämäärän ja ajan tarkastelemista tietokonejärjestelmässä. Tämä on tärkeää ohjelmoijille, koska se mahdollistaa aikaleimojen lisäämisen tiedostoihin ja tietojen tallentamisen tietokantaan.

## Miten:

Seuraavassa on esimerkkejä siitä, miten saat nykyisen päivämäärän Java-ohjelmassa:

```Java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        // Nykyinen päivämäärä
        LocalDate currentDate = LocalDate.now();
        System.out.println("Nykyinen päivämäärä: " + currentDate);

        // Nykyinen vuosi
        int currentYear = currentDate.getYear();
        System.out.println("Nykyinen vuosi: " + currentYear);
    }
}
```

Esimerkkilähtö:

```
Nykyinen päivämäärä: 2020-11-23
Nykyinen vuosi: 2020
```

## Syvemmälle:

Päivämäärän saamisen toiminto on osa Java-kirjastoa nimeltä ```java.time```. Tämä kirjasto sisältää paljon erilaisia ​​toimintoja päivämäärän ja ajan hallintaan. On myös olemassa muita tapoja saada nykyinen päivämäärä, kuten käyttämällä ```java.util.Date``` -luokkaa tai käyttämällä järjestelmän kellon aikaa ```System.currentTimeMillis()```.

## Katso myös:

- Java 8 - Oppikirjan päivämäärä ja aika (https://docs.oracle.com/javase/tutorial/datetime/overview/index.html)
- W3Schools - Java Date and Time (https://www.w3schools.com/java/java_date.asp)