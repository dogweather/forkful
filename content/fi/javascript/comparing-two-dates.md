---
title:                "Kahden päivämäärän vertailu"
html_title:           "Javascript: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärien vertailu on tapa verrata kahta päivämäärää keskenään ja nähdä, kumpi niistä on suurempi tai pienempi. Tämä on tärkeää ohjelmoinnissa, sillä päästään tarkastelemaan aikaa ja päivämääriä sekä tekemään päätöksiä niiden perusteella.

## Miten tehdä:

Päivämäärien vertailu on helppoa Javascriptissä käyttämällä Date-objektia. Voit luoda kaksi päivämäärä-objektia ja käyttää vertailuoperaattoreita nähdäksesi, kumpi niistä on suurempi tai pienempi. Katso alla olevat koodiesimerkit ja niiden tulosteet:

```javascript
let date1 = new Date('2021-05-01');
let date2 = new Date('2021-05-05');

console.log(date1 < date2); // Output: true
console.log(date1 > date2); // Output: false
```
Huomaa: Vertailuoperaattorien tulos on aina boolean-arvo (true tai false), joten niitä voi hyödyntää myös ehtolauseissa.

## Syvällistä tietoa:

Päivämäärien vertailulla on pitkä historia ajankäsitteen ymmärtämisessä ja sen tarkkuudessa. Javascriptin vanhemmat versiot saattoivat aiheuttaa ongelmia päivämäärien vertailussa, mutta nykyään Date-objektin käyttö on suositeltavaa.

Joskus on myös hyödyllistä vertailla päivämääriä millisekuntien tarkkuudella. Tähän tarkoitukseen voi käyttää Date-objektin getTime()-metodia, joka palauttaa millisekunnit tiettyyn päivämäärään verrattuna.

## Katso myös:

- [Javascript Date - MDN Docs](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [History of Time](https://www.timeanddate.com/time/intro.html)