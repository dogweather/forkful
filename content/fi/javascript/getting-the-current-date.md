---
title:                "Nykyisen päivämäärän selvittäminen"
html_title:           "Javascript: Nykyisen päivämäärän selvittäminen"
simple_title:         "Nykyisen päivämäärän selvittäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
Joskus ohjelmissa on tarve saada nykyinen päivämäärä selville, esimerkiksi muistutuksia varten tai datan järjestämistä varten. Tässä artikkelissa opit, kuinka saat nykyisen päivämäärän käyttämällä Javascriptiä.

## Miten
Onneksi Javascriptillä on valmiina Date-objekti, joka helpottaa nykyisen päivämäärän saamista. Voit käyttää sitä seuraavasti:

```Javascript
let currentDate = new Date();
console.log(currentDate);
```

Tämä tulostaa nykyisen päivämäärän ja ajan muodossa "päivä kuukausi vuosi tunti:minuutti:sekunti GMT+0200 (Itä-Euroopan kesäaika)". Voit myös eriyttää päivämäärän, kuukauden ja vuoden erikseen käyttämällä Date-objektin metodeja:

```Javascript
let day = currentDate.getDate();
let month = currentDate.getMonth() + 1; // huomaa että kuukaudet aloittuvat 0:sta 
let year = currentDate.getFullYear();
console.log(`${day}.${month}.${year}`);
```

Tämä tulostaa esimerkiksi "15.7.2020". Voit myös näyttää päivämäärän eri muodoissa, esimerkiksi englanniksi käyttämällä Date-objektin metodia `toDateString()`:

```Javascript
console.log(currentDate.toDateString());
```

Tämä tulostaa nykyisen päivämäärän muodossa "Wed Jul 15 2020". Date-objektin avulla voit myös tarkistaa, onko kyseinen vuosi karkausvuosi `getFullYear()` ja `getMonth()` metodeiden avulla.

## Syvemmällä
Date-objektin sisältämää tietoa voi käyttää monipuolisesti eri ohjelmointitarkoituksiin. Voit esimerkiksi laskea eron kahden päivämäärän välillä ja näyttää sen millisekunteina käyttämällä `getTime()` metodeita. Voit myös asettaa ja muokata päivämäärää käyttämällä Date-objektin eri metodeja kuten `setFullYear()` tai `setMonth()`. Lisätietoja löydät Javascriptin virallisesta dokumentaatiosta.

## Katso myös
- [Javascriptin virallinen dokumentaatio](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schoolsin opas Date-objektin käyttöön](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Date-objektin esimerkkejä ja tehtäviä](https://www.javatpoint.com/object-date-javascript)