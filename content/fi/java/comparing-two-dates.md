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

## Mitä & Miksi?

Vertaaminen kahden päivämäärän välillä on ohjelmoinnissa yhteinen tehtävä. Se tarkoittaa yksinkertaisesti kahden päivämäärän vertailua selvittääkseen kumpi niistä on ennen tai jälkeen toisen päivämäärän. Ohjelmoijat tekevät tätä usein esimerkiksi tarkistaakseen, onko asiakkaan sopimus voimassa tai onko tietty päivämäärä tulevaisuudessa.

## Kuinka tehdä se:

```Java 
// Oletetaan, että haluamme verrata kahta päivämäärää: 01/01/2020 ja 05/01/2020
LocalDate date1 = LocalDate.of(2020, 1, 1);
LocalDate date2 = LocalDate.of(2020, 5, 1);

// Verrataan päivämääriä käyttäen compareTo() metodia
int result = date1.compareTo(date2);

if (result < 0) {
    System.out.println("date1 on ennen date2");
} else if (result > 0) {
    System.out.println("date1 on jälkeen date2");
} else {
    System.out.println("date1 ja date2 ovat samat");
}

// Output: date1 on ennen date2
```

## Syväsukellus:

Päivämäärien vertailu on ollut tärkeä tehtävä jo pitkään, ja Java tarjoaa monia tapoja tehdä se. Yksi vaihtoehto on käyttää compareTo() metodia, kuten yllä olevassa esimerkissä. Toinen vaihtoehto on käyttää isEqual() tai isBefore() ja isAfter() metodeja. Näillä metodeilla voit tarkistaa, ovatko päivämäärät samat tai ovatko ne ennen tai jälkeen toisiaan.

## Katso myös:

- [Java LocalDate API](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Ohjeita päivämäärän vertailuun Java:ssa](https://www.baeldung.com/java-date-compare)
- [Java 8 DateTime API opetusohjelma](https://www.baeldung.com/java-8-date-time-intro)