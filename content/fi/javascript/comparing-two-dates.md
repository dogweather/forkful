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

## Miksi

Aloitetaan kysymyksestä, miksi joku haluaisi verrata kahta päivämäärää Javascriptillä. Yksinkertaisesti sanottuna, päivämäärien vertailu on tärkeää, kun halutaan tarkistaa onko jokin tapahtuma tapahtunut ennen toista tai onko kaksi tapahtumaa tapahtunut samana päivänä.

## Miten tehdä

Ensimmäinen askel on luoda kaksi Date-objektia, jotka sisältävät vertailtavat päivämäärät. Tämän jälkeen voimme käyttää Date-objektien sisäänrakennettuja vertailumetodeja, kuten `getTime()` tai `valueOf()`, joiden avulla voimme vertailla päivämääriä millisekunneiksi tai millisekuntitimanteiksi.

Esimerkiksi, haluamme tarkistaa onko ensimmäinen päivämäärä ennen toista. Voimme tehdä tämän seuraavasti:

```Javascript
let date1 = new Date('2020-10-01'); // luodaan ensimmäinen Date-objekti
let date2 = new Date('2021-10-01'); // luodaan toinen Date-objekti

if(date1.getTime() < date2.getTime()) { // vertaillaan millisekunneiksi
  console.log(date1 + ' on ennen ' + date2);
} else {
  console.log(date1 + ' ei ole ennen ' + date2);
}
```

Tässä tapauksessa tulostamme päivämäärän `date1`, koska sen aikaleima on pienempi kuin `date2`:n aikaleima.

Voimme myös tarkistaa ovatko kaksi päivämäärää yhtä suuria käyttämällä `valueOf()`-metodia:

```Javascript
let date1 = new Date('2020-10-01'); // luodaan ensimmäinen Date-objekti
let date2 = new Date('2020-10-01'); // luodaan toinen Date-objekti

if(date1.valueOf() === date2.valueOf()) { // vertaillaan millisekuntitimanteiksi
  console.log(date1 + ' ja ' + date2 + ' ovat samat päivämäärät.');
} else {
  console.log(date1 + ' ja ' + date2 + ' eivät ole samat päivämäärät.');
}
```

Tässä tapauksessa tulostamme, että molemmat päivämäärät ovat samat.

## Syvemmälle tarkasteluun

Kun vertaillaan päivämääriä Javascriptillä, on tärkeää huomioida, että Date-objekteilla on tarkkuus vain millisekunteihin asti. Tämä tarkoittaa sitä, että kaksi päivämäärää, jotka ovat samassa minuutissa ja sekunnissa mutta eri millisekunnissa, katsotaan eri päivämääriksi.

Lisäksi, Date-objektit käyttävät UTC-aikaa, joten tuloksissa saattaa olla eroja, jos käyttäjän tai selaimen aikavyöhyke vaihtelee.

On myös hyvä huomata, että Javascriptia käytettäessä on suositeltavaa käyttää vertailussa millisekuntitimantteja `valueOf()`-metodin sijaan, sillä tämä antaa tarkemman ja luotettavamman tuloksen.

## Katso myös

- [MDN: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Date-fns](https://date-fns.org/)