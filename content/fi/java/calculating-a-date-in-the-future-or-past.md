---
title:                "Määritetään päivämäärä tulevaisuudessa tai menneisyydessä"
html_title:           "Java: Määritetään päivämäärä tulevaisuudessa tai menneisyydessä"
simple_title:         "Määritetään päivämäärä tulevaisuudessa tai menneisyydessä"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän laskeminen tulevaiseen tai menneeseen on tavallinen tehtävä ohjelmoinnissa. Se tarkoittaa tietyn päivämäärän lisäämistä tai vähentämistä tietyn aikamäärän verran. Ohjelmoijat tekevät tätä esimerkiksi pysyvien muistutusten luomiseksi tai tietokantaan tallennettujen päivämäärien käsittelyä varten.

## Näin teet:
```Java
// Esimerkki päivämäärän lisäämisestä 5 päivää eteenpäin
LocalDate tanaan = LocalDate.now();
LocalDate viidenPaivanPaasta = tanaan.plusDays(5);
System.out.println(viidenPaivanPaasta); // 2021-04-20
```

```Java
// Esimerkki päivämäärän vähentämisestä 2 kuukautta taaksepäin
LocalDate tanaan = LocalDate.now();
LocalDate kaksiKuukauttaTaakse = tanaan.minusMonths(2);
System.out.println(kaksiKuukauttaTaakse); // 2021-02-20
```

## Syvällisempi sukellus:
Päivämäärän laskemisen käyttö on ollut osa ohjelmointia jo pitkään, mutta nykyaikaisilla ohjelmointikielillä, kuten Javalla, siitä on tullut yksinkertaisempaa. On myös muita tapoja käsitellä päivämääriä, kuten käyttämällä kalenteriluokkia tai kolmannen osapuolen kirjastoja.

Päivämäärien laskemiseen käytetään usein päivämääräolioita, jotka sisältävät päivämäärän ja ajan tiedot. Java 8 toi mukanaan uuden LocalDate-luokan, joka mahdollistaa päivämäärän käsittelyn ilman aikatietoja. Tämä helpottaa monia yleisiä päivämäärämanipulaatioita ja on myös suorituskykyisempi kuin vanhemmat kalenteriin perustuvat ratkaisut.

## Katso myös:
Java 8 LocalDate-dokumentaatio: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html