---
title:                "Kahden päivämäärän vertailu"
html_title:           "TypeScript: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Päivämäärien vertailu on tärkeä osa ohjelmointia, joka mahdollistaa tulevaisuuden ja menneisyyden päivämäärien vertailun. Tämä on erityisen tärkeää aikaperusteisten tehtävien suorittamisessa, kuten tapahtumien aikajärjestyksen määrittämisessä.

## Miten:
Vertaillessamme kahta päivämäärää, meidän on ensin muunnettava ne samassa muodossa oleviksi päivämääriksi. Tämän jälkeen voimme käyttää valmiita funktioita, kuten Date.getTime (), jotka muuttavat päivämäärän millisekunneiksi ja helpottavat vertailua.
Esimerkki:
```
TypeScript
let date1 = new Date(2020, 1, 1);
let date2 = new Date(2020, 2, 1);
if (date1.getTime() < date2.getTime()) {
  console.log("Date1 is before Date2");
}
```
Tulos:
```
Date1 is before Date2
```

## Syventävä sukellus:
Päivämäärien vertailu on ollut tärkeä osa ohjelmointia jo pitkään, ja eri kielissä on erilaisia tapoja käsitellä päivämäärien vertailua. Esimerkiksi Java-kielessä on käytössä Calendar-luokka, joka tarjoaa monipuolisia toimintoja päivämäärien käsittelyyn. TypeScriptissä puolestaan voidaan käyttää Date-objektia ja sen valmiita funktioita vertailuun.
Joskus päivämäärien vertailu voi olla monimutkaista, sillä päivämäärien tarkkuus vaihtelee eri maissa ja kulttuureissa. On tärkeää ottaa tämä huomioon, jotta vältetään virheellisten tulosten saaminen.

## Katso myös:
Lisätietoa päivämäärien vertailusta TypeScriptissä löydät seuraavista lähteistä:
- [TypeScriptin virallinen dokumentaatio](https://www.typescriptlang.org/docs/)
- [MDN - Date-objekti](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Stack Overflow - Comparing two dates in TypeScript](https://stackoverflow.com/questions/31287320/comparing-two-dates-in-typescript)