---
title:                "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
html_title:           "Java: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi
Ihmiset usein tarvitsevat laskutoimituksia tulevien tai menneiden päivien laskemiseen Java-ohjelmoinnissa. Tämä voi olla tarpeen esimerkiksi aikataulujen laatimisessa tai tiettyjen päivien välisen aikaeron laskemisessa.

## Kuinka
Käytä Java-koodia seuraavien esimerkkien avulla laskeaksesi päivämäärän tulevaisuudessa tai menneisyydessä.

```
// Tulevaisuudessa
LocalDate tulevaisuus = LocalDate.now().plusDays(7);
System.out.println(tulevaisuus);
// Output: 2022-01-07

// Menneisyydessä
LocalDate menneisyys = LocalDate.now().minusYears(2);
System.out.println(menneisyys);
// Output: 2019-01-14
```

## Syväsyventyminen
Java-ohjelmassa päivien laskeminen tulevaisuuteen tai menneisyyteen voidaan tehdä LocalDate-luokan avulla, joka sisältää monia käteviä metodeja päivämäärien muokkaamiseen. Tulevaisuudessa päiviä voidaan lisätä käyttämällä plusDays (), plusMonths () tai plusYears () -metodeja, kun taas menneisyydessä päivistä voidaan vähentää käyttämällä vastaavia miinus-metodeja. Lisäksi voidaan käyttää myös muita metodeja, kuten withDayOfMonth () tai withYear (), jotka mahdollistavat päivän tai vuoden tarkan asettamisen.

## Katso myös
- [Java LocalDate -dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Date and Time API -opas](https://docs.oracle.com/javase/tutorial/datetime/index.html)
- [Java 8: n uudet aikapinnatutoriaalit](https://www.baeldung.com/java-8-date-time-intro)