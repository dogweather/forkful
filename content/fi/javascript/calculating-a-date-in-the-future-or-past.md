---
title:                "Javascript: Tulevaisuuden tai menneen päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneen päivämäärän laskeminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi laskea tulevaisuuden tai menneisyyden päivämäärän? On monia tapauksia, joissa tämä toiminto olisi hyödyllinen, kuten laskut, aikataulujen luominen tai yksinkertaisesti kuriositeettina.

## Näin teet sen

Laskeminen tulevaisuuden tai menneisyyden päivämäärää JavaScriptillä on suhteellisen helppoa. Tarvitset vain Date-olion ja haluamasi päivien määrän, jonka haluat lisätä (tulevaisuutta) tai vähentää (menneisyyttä). Esimerkiksi, jos haluat lisätä 10 päivää nykyiseen päivään, voit käyttää seuraavaa koodia:

```Javascript
const nykyinenPaiva = new Date();
nykyinenPaiva.setDate(nykyinenPaiva.getDate() + 10);
console.log(nykyinenPaiva);
```

Tämä koodi luo uuden Date-olion nykyisestä päivästä ja lisää siihen 10 päivää. Tulostus näyttää tulevaisuuden päivämäärän, joka on 10 päivää nykyisen päivän jälkeen.

Voit myös vähentää päiviä muuttamalla plussamerkin miinusmerkiksi ```nykyinenPaiva.getDate() - 10```.

## Syvemmälle

Date-olioon liittyy monia hyödyllisiä metodteja, kuten getDate(), getMonth(), ja getFullYear(), jotka palauttavat päivämäärän, kuukauden ja vuoden. Voit myös käyttää näitä metodeja laskeaksesi haluamasi päivämäärän tarkalleen.

Lisäksi, voit käyttää myös muotoilusymboleja, kuten ```%d``` ja ```%m```, tulostamaan päivämäärät ja kuukaudet numeroina. Tämä voi olla hyödyllistä, jos haluat luoda aikatauluja tai otteluita varten.

Voit myös käyttää Date-olion toista konstruktoria asettaaksesi tietyn päivämäärän ja ajan. Esimerkiksi, voit asettaa tulevaisuuden päivämäärän ja ajan seuraavasti:

```Javascript
const tulevaPaiva = new Date(2022, 4, 2, 13, 30, 0);
console.log(tulevaPaiva);
```

Tämä koodi luo Date-olion, joka edustaa tulevaa päivämäärää ja aikaa 2. toukokuuta, 2022 klo 13:30. Huomaa, että kuukaudet aloittavat numerosta 0, joten tammikuu on 0 ja joulukuu on 11.

## Katso myös

- [MDN: Date](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScript.info: Date and time](https://javascript.info/date)
- [W3Schools: JavaScript Date Object](https://www.w3schools.com/js/js_dates.asp)