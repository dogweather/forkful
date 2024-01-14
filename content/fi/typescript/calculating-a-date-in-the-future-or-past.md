---
title:                "TypeScript: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Laskemalla tulevaisuuden tai menneisyyden päivämäärää voi olla hyödyllistä esimerkiksi tapahtumien suunnittelussa tai aikataulujen laatimisessa.

## Ohjeet

Laskeminen tulevaisuuden tai menneisyyden päivämäärää TypeScript-koodissa on helppoa. Käytä ensin JavaScriptin Date-objektia luomaan haluttu päivämäärä. Sitten voit käyttää Date-objektin sisäänrakennettuja metodeja, kuten `setFullYear()` tai `getFullYear()` muokatakseen tai palauttaakseen vuoden. Esimerkiksi:

```TypeScript
let tulevaisuudenPaiva = new Date();
tulevaisuudenPaiva.setFullYear(2021);
console.log(tulevaisuudenPaiva); // output: Fri Jun 25 2021 12:00:00 GMT+0300 (Eastern European Summer Time)

let menneisyydenPaiva = new Date();
menneisyydenPaiva.setFullYear(1990);
console.log(menneisyydenPaiva); // output: Wed Jun 25 1990 12:00:00 GMT+0300 (Eastern European Summer Time)
```

## Syvempi sukellus

Date-objektin metodeilla on useita eri parametreja, joilla voit muokata päivämäärää tarkemmin. Esimerkiksi voit käyttää `setDate()`-metodia asettaaksesi tietyn päivän kuukaudesta tai `getMonth()`-metodia saadaksesi kuukauden numeron sijasta kuukauden nimen. Voit myös käyttää `getTime()`-metodia saadaksesi päivämäärän millisekunneissa laskettuna. Tutustu JavaScriptin dokumentaatioon saadaksesi lisätietoja näistä ja muista Date-objektin metodeista.

## Katso myös

- [Date-objektin dokumentaatio JavaScriptissä (MDN)](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)