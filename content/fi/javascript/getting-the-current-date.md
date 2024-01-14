---
title:                "Javascript: Päivämäärän hakeminen"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi Hakea Nykyinen Päivämäärä?

Javascriptissä on monia tilanteita, joissa tarvitsemme tietoa nykyisestä päivämäärästä. Esimerkiksi voi olla tarpeen näyttää käyttäjälle ajankohtaista tietoa tai tallentaa jokin toimenpide tietokantaan tietyn päivämäärän mukaan. Tässä blogikirjoituksessa käymme läpi, miten voit helposti hakea nykyisen päivämäärän Javascriptillä.

## Miten Tehdä Se?

Onneksi Javascriptillä on sisäänrakennettu Date-objekti, jolla voimme helposti hakea nykyisen päivämäärän. Voit luoda uuden Date-objektin joko pelkällä Date-sanaa käyttäen tai antamalla sille parametrina halutun vuoden, kuukauden ja päivän. Alla olevissa esimerkeissä näytämme molemmat vaihtoehdot.

```Javascript
// Hakee nykyisen päivämäärän
var nykyinenPäivä = new Date();
console.log(nykyinenPäivä); // Tulostaa esimerkiksi Wed Sep 29 2021 09:56:15 GMT+0300 (Eastern European Summer Time)

// Hakee tietyn päivämäärän vuoden, kuukauden ja päivän mukaan
var haluttuPäivä = new Date(2021, 8, 29); // Huomaa, että kuukaudet aloitetaan nollasta (tammikuu on 0, helmikuu 1 jne.)
console.log(haluttuPäivä); // Tulostaa Wed Sep 29 2021 00:00:00 GMT+0300 (Eastern European Summer Time)
```

Voit myös hakea tiettyjä osia päivämäärästä erikseen. Alla olevassa esimerkissä näytämme, miten voit hakea päivän, kuukauden ja vuoden erikseen.

```Javascript
var nykyinenPäivä = new Date();

console.log(nykyinenPäivä.getDate()); // Tulostaa nykyisen päivämäärän numerona (esimerkiksi 29)
console.log(nykyinenPäivä.getMonth()); // Tulostaa nykyisen kuukauden numerona (tässä tapauksessa 8, koska kuukaudet aloitetaan nollasta)
console.log(nykyinenPäivä.getFullYear()); // Tulostaa nykyisen vuoden numerona (esimerkiksi 2021)
```

## Syvä Sukellus

Date-objektilla on monia muitakin hyödyllisiä metodeja, jotka voivat auttaa sinua työskennellessäsi päivämäärien kanssa. Voit esimerkiksi hakea päivämäärän tietyn aikavälin päästä tai tarkastaa, onko vuosi karkausvuosi. Suosittelemme tutustumaan lisää Date-objektin dokumentaatioon, jotta voit käyttää sitä täysin hyödyksi.

## Katso Myös

- [Javascript Date - Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Date Object - W3Schools](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Päivämäärän Haku - Codecademy (englanniksi)](https://www.codecademy.com/blog/77-p%C3%A4iv%C3%A4m%C3%A4%C3%A4r%C3%A4n-haku-javascriptill%C3%A4)