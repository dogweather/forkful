---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Javascript: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Halutessasi tulostaa päivämäärän tekstiksi sinun täytyy ensin muuntaa se asianmukaiseen muotoon, jotta voit näyttää sen haluamallasi tavalla.

## Kuinka
Javascriptissä on useita tapoja muuntaa päivämäärä merkkijonoksi. Alla on esimerkkikoodia siitä, kuinka tämä voidaan tehdä käyttämällä Moment.js-kirjastoa:

````Javascript
// Määritä päivämäärä
let date = new Date();
// Muuta päivämäärä merkkijonoksi
let dateString = moment(date).format("DD/MM/YYYY");
// Tulosta muunnettu päivämäärä
console.log(dateString);
````

Tämä tuottaa tulosteen "12/05/2021".

Toinen tapa muuntaa päivämäärä merkkijonoksi on käyttää Date-objectin sisäänrakennettua `toLocaleDateString()`-metodia:

````Javascript
// Määritä päivämäärä
let date = new Date();
// Muuta päivämäärä merkkijonoksi
let dateString = date.toLocaleDateString("fi-FI");
// Tulosta muunnettu päivämäärä
console.log(dateString);
````

Tämä tuottaa tulosteen "12.5.2021".

## Syvällinen sukellus
Date-objectissa on myös muita hyödyllisiä metodeita, joilla voit muuntaa päivämäärän haluamaasi muotoon. Esimerkiksi `getDay()`-metodi palauttaa päivän viikonpäivän numerona, `getMonth()`-metodi kuukauden numerona ja `getFullYear()`-metodi vuoden numerona. Näitä voit käyttää yhdessä esimerkiksi taulukon avulla, josta voit tarkastaa, mikä päivämäärä vastaa tiettyä numeroa.

## Katso myös
- [Moment.js-dokumentaatio](https://momentjs.com/docs/)
- [Date Object - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)