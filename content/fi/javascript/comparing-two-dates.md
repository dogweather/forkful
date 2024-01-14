---
title:    "Javascript: Kahden päivämäärän vertailu"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnissa täytyy tarkastella kahta päivämäärää ja verrata niitä keskenään. Tämä voi olla tarpeellista esimerkiksi aikaleimojen käsittelyssä tai tapahtumien järjestämisessä. Tässä blogikirjoituksessa käymme läpi, miten voit helposti vertailla päivämääriä Javascript-ohjelmoinnissa.

## Miten

Vertaillaaksemme kahta päivämäärää Javascript-ohjelmoinnissa, meidän täytyy ensin muuttaa ne Date-tyyppisiksi objekteiksi. Tämän jälkeen voimme käyttää erilaisia vertailutoimintoja, kuten isEqual(), isBefore() ja isAfter(), verrataksemme näitä päivämääriä.

Esimerkiksi jos haluamme tietää onko ensimmäinen päivämäärä ennen toista, voimme käyttää isBefore() -funktiota seuraavasti:

```Javascript
var date1 = new Date("2020-11-01");
var date2 = new Date("2020-11-07");

console.log(date1.isBefore(date2));
```

Tämä tulostaa "true", koska date1 on aikaisempi kuin date2. Voit myös käyttää isEqual() tai isAfter() -funktioita saadaksesi vastaavan tiedon.

## Syväsukellus

Vertailutoimintojen lisäksi voimme myös käyttää Date-objektien sisäisiä metodeita, kuten getTime() ja getFullYear(), vertaillaksemme päivämääriä. getTime() palauttaa millisekuntien määrän, joka on kulunut tietystä päivämäärästä, ja getFullYear() palauttaa kyseisen vuoden.

Esimerkiksi jos haluat vertailla kahden päivämäärän vuosia, voit käyttää seuraavaa koodia:

```Javascript
var date1 = new Date("2020-11-01");
var date2 = new Date("2019-12-31");

if (date1.getFullYear() === date2.getFullYear()) {
  console.log("Samana vuonna.");
} else {
  console.log("Eri vuodet.");
}
```

Tämä tulostaisi "Eri vuodet", koska date1 ja date2 ovat eri vuosiluvuilla.

## Katso myös

- [MDN web docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: JavaScript Dates](https://www.w3schools.com/js/js_dates.asp)
- [JavaScript.info: Date and time](https://javascript.info/date)