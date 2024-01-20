---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Päivämäärän vertaaminen tarkoittaa kahden päivämäärän suhteellisen ajanhetken selvittämistä toisiinsa nähden. Tämä on tärkeää, jotta voit esimerkiksi kontrolloida tapahtumasarjoja tai laskea ajanjaksoja sovelluksissasi.

## Näin teet:
Alla on esimerkki Javascriptin Date-objektin käytöstä päivämäärän vertaamiseen.

```Javascript
let date1 = new Date(2020, 11, 31);
let date2 = new Date(2021, 0, 1);

if (date1 < date2) {
  console.log("date1 on aikaisempi kuin date2");
} else if (date1 > date2) {
  console.log("date1 on myöhempi kuin date2");
} else {
  console.log("date1 ja date2 ovat samassa ajan hetkessä");
}
```

Tämä tuottaa tulosteen: "date1 on aikaisempi kuin date2"

## Syvempi syöksy:
Päivämäärien vertaaminen on ollut oleellinen osa ohjelmointia sen alkuajoista lähtien, ja se on tärkeä osa tapahtumien ajoitusta ja aikaisempien tietojen analysointia.

Vaihtoehtoisesti, voit käyttää getTime() -metodia, joka palauttaa ajan millisekunteina vuodesta 1970, 1.tammikuuta. Tämä on toinen tapa vertailla päivämääriä.

```Javascript
let date1 = new Date(2020, 11, 20);
let date2 = new Date(2021, 0, 1);

if (date1.getTime() < date2.getTime()) {
  console.log("date1 on aikaisempi kuin date2");
} else if (date1.getTime() > date2.getTime()) {
  console.log("date1 on myöhempi kuin date2");
} else {
  console.log("date1 ja date2 ovat samassa ajan hetkessä");
}
```

Parse- ja ValueOf -metodit tarjoavat myös erilaisia vaihtoehtoja päivämäärän vertailuun.

## Lisätietoja:
- [JavaScript Date Objekti](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Aikaleimat JavaScriptissä](https://www.w3schools.com/js/js_date_methods.asp)