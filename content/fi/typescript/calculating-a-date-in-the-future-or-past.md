---
title:                "TypeScript: Tulevien tai menneiden päivämäärien laskeminen"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi lasketaan päivämäärä tulevaisuudessa tai menneisyydessä?

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä voi olla hyödyllistä monissa ohjelmointitilanteissa. Esimerkiksi tiettyä tapahtumaa tai deadlinea varten halutaan saada tieto siitä, kuinka monen päivän päästä tai kuinka monen päivän takana tämä päivämäärä on. Tai ehkä tarkoituksena on laskea henkilön ikä tietynä päivänä tai tarkastella tiettyjen päivämäärien välistä aikaväliä. Tällaisissa tilanteissa päivämäärän laskeminen tulevaisuudessa tai menneisyydessä voi olla hyödyllistä ja säästää aikaa ja vaivaa.

## Miten lasketaan päivämäärä tulevaisuudessa tai menneisyydessä?

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä on helppoa käyttäen TypeScript ohjelmointikieltä. Alla on esimerkki koodista, jossa lasketaan tietyn päivämäärän päivien määrä tulevaisuuteen tai menneisyyteen.

```TypeScript
// Alkuperäinen päivämäärä 1.1.2020
let date = new Date(2020, 0, 1);

// Lasketaan päivä eteenpäin 10 päivää
date.setDate(date.getDate() + 10);
console.log(date); // Output: Wed Jan 11 2020

// Lasketaan päivä taaksepäin 5 päivää
date.setDate(date.getDate() - 5);
console.log(date); // Output: Fri Jan 6 2020
```

Kuten koodista nähdään, uusi päivämäärä saadaan laskettua muuttamalla alkuperäisen päivämäärän päivämäärää `setDate()` funktion avulla. Päivien määrä muuttuu sen mukaan, kuinka monta päivää lisätään tai vähennetään alkuperäisestä päivämäärästä.

## Syvällisempi tarkastelu päivämäärän laskemisesta tulevaisuudessa tai menneisyydessä

Päivämäärän laskemisessa tulevaisuudessa tai menneisyydessä on tärkeää huomioida erilaiset aikavyöhykkeet ja kesäaika. Esimerkiksi tietokoneen asetukset ja sijainti voivat vaikuttaa päivämäärän laskemiseen. Siksi on tärkeää varmistaa, että aikavyöhykkeet ja kesäaika ovat oikein asetettuna ennen päivämäärän laskemista.

Lisäksi TypeScriptissä on käytettävissä muitakin päivämäärää ja aikaa käsitteleviä toimintoja, kuten `getFullYear()` ja `getMonth()` funktiot. Näitä kannattaa hyödyntää, jos halutaan tarkemmin manipuloida päivämäärää tulevaisuudessa tai menneisyydessä.

## Katso myös

- [Date API Reference by Mozilla](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Date and Time Operations in TypeScript](https://www.tutorialspoint.com/typescript/typescript_date.htm)
- [Handling Dates and Times in TypeScript](https://medium.com/@roszczux/handling-dates-and-times-in-typescript-cf257538a9af)