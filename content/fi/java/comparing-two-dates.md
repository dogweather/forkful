---
title:                "Kahden päivämäärän vertailu"
html_title:           "Java: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnissa tarvitaan tapaa vertailla kahta päivämäärää keskenään, esimerkiksi tarkistamaan onko jokin tapahtuma mennyt jo ohi tai laskemaan aikaeroja. Joissakin tapauksissa myös päivämäärien järjestäminen tai ryhmittely voi vaatia päivämäärien vertailua.

## Ohjeet

Vertaillessa päivämääriä Java-ohjelmassa on tärkeää muistaa tarkistaa myös ajallisesti päivämäärien järjestys. Tässä on esimerkki koodista, joka vertaa kahta päivämäärää:

```Java
import java.time.*;

LocalDate firstDate = LocalDate.of(2020, 3, 15); // Ensimmäinen päivämäärä
LocalDate secondDate = LocalDate.of(2020, 3, 18); // Toinen päivämäärä

if(firstDate.isBefore(secondDate)){
  System.out.println("Ensimmäinen päivämäärä on ennen toista päivämäärää.");
} else if(firstDate.isAfter(secondDate)){
  System.out.println("Toinen päivämäärä on ennen ensimmäistä päivämäärää.");
} else {
  System.out.println("Päivämäärät ovat samat.");
}

// Tulostus: Ensimmäinen päivämäärä on ennen toista päivämäärää.
```

Tässä esimerkissä käytämme Java 8:ssa esiteltyä uutta LocalDate-luokkaa, joka helpottaa päivämäärien käsittelyä ja vertailua. Koodissa käytämme myös if-lauseita tarkistamaan päivämäärien järjestyksen ja tulostamme sen mukaisen viestin.

## Syvällinen sukellus

Java:ssa päivämäärien vertailu tapahtuu pääasiassa LocalDate-luokan avulla, kuten yllä olevassa esimerkissä näimme. Tämä luokka sisältää erilaisia metodeita päivämäärien vertailuun, kuten `isBefore()` ja `isAfter()`. Lisäksi LocalDaten lisäksi Java sisältää myös muita päivämääriä käsitteleviä luokkia, kuten LocalDateTime ja ZonedDateTime.

Jos haluat tutustua syvemmin päivämäärien vertailuun Java-ohjelmassa, suosittelemme lukemaan Java:n virallista dokumentaatiota sekä kokeilemaan erilaisia vertailukeinoja käytännössä.

## Katso myös

- [Java:n virallinen dokumentaatio](https://docs.oracle.com/javase/8/docs/api/)
- [LocalDate-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutoriaali päivämäärien käyttämisestä Java:ssa](https://www.baeldung.com/java-date-compare)
- [Stack Overflow-kysymys päivämäärien vertailusta Java:ssa](https://stackoverflow.com/questions/20061783/comparing-two-dates-in-java)