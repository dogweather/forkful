---
title:                "Tulevan tai menneen päivämäärän laskeminen tietokoneohjelmoinnilla"
html_title:           "Javascript: Tulevan tai menneen päivämäärän laskeminen tietokoneohjelmoinnilla"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen tietokoneohjelmoinnilla"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoinnissa on tarpeen laskea tietty päivämäärä tulevaisuuteen tai menneisyyteen. Tämä voi olla esimerkiksi kalenterinäkymän luomista varten tai laskurin toiminnan optimoimiseksi. Javascriptillä tämä on onneksi hyvin yksinkertaista!

## Miten

Laskeminen tulevaisuuden tai menneisyyden päivämääräksi Javascriptillä vaatii muutaman askeleen. Ensimmäiseksi tarvitsemme nykyisen päivämäärän, jota varten käytämme `new Date()` -funktiota. Tämän jälkeen voimme käyttää `setDate()` -metodia muuttamaan tätä päivämäärää halutulla tavalla. Lopuksi muotoilemme päivämäärän halutun näkyvän muodon mukaiseksi. Alla on esimerkki tulevaisuuden päivämäärän laskemisesta 7 päivää nykyisestä päivämäärästä.

```Javascript
// Luodaan muuttuja nykyiselle päivämäärälle
let currentDate = new Date();

// Muutetaan päivämäärää lisäämällä 7 päivää
currentDate.setDate(currentDate.getDate() + 7);

// Muotoillaan päivämäärä halutunlaiseksi merkkijonoksi
let formattedDate = `${currentDate.getDate()}.${currentDate.getMonth() + 1}.${currentDate.getFullYear()}`;

console.log(formattedDate); // Tulostaa esimerkiksi: 11.8.2021
```

## Syvällinen sukellus

Javascriptillä päivämäärän laskeminen tulevaisuuteen tai menneisyyteen perustuu siis `Date`-tyyppiseen olioon. Tämä olio sisältää useita hyödyllisiä metodeja, kuten `setDate()`, `setMonth()` ja `setFullYear()` päivämäärän muokkaamiseen. Lisäksi `getDate()`, `getMonth()` ja `getFullYear()` -metodeilla voidaan palauttaa päivämäärän osia. On myös hyvä ottaa huomioon, että kuukaudet aloittuvat Javascriptissä indeksistä 0, joten tammikuu on arvoltaan 0 ja joulukuu arvoltaan 11.

## Katso myös

- [MDN: Date - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3schools: JavaScript Dates](https://www.w3schools.com/js/js_dates.asp)
- [JavaScript.info: Date and time](https://javascript.info/date)