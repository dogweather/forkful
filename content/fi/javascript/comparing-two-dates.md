---
title:                "Javascript: Kahden päivämäärän vertailu"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Miksi vertailla kaksi päivämäärää?

On monia syitä, miksi voisi olla tarpeellista verrata kahta eri päivämäärää. Se voi auttaa meitä laskemaan aikavälejä, tarkistamaan päivämääriä ja jopa tehdä päätöksiä tulevista tapahtumista. Onneksi Javascript tarjoaa meille helpon tavan tehdä tämä vertailu.

## Kuinka vertailla päivämääriä Javascriptillä

Voit vertailla kahta päivämäärää käyttämällä `Date`-objekteja ja vertailuoperaattoreita. Tässä on yksinkertainen esimerkki:

```Javascript
const date1 = new Date("2021-01-01");
const date2 = new Date("2020-12-31");

if (date1 > date2) {
  console.log("Date 1 is later than Date 2");
} else if (date1 < date2) {
  console.log("Date 1 is earlier than Date 2");
} else {
  console.log("Date 1 is equal to Date 2");
}
```

Tässä esimerkissä luomme kaksi päivämääräobjektia ja vertailemme niitä `>` ja `<` operaattoreilla. Voit myös käyttää muita operaattoreita, kuten `>=` ja `<=` riippuen tarpeistasi.

Voit myös käyttää `getTime()`-metodia, joka palauttaa päivämäärän millisekunteina. Tämä on hyödyllistä, jos haluat vertailla päivämääriä tarkemmin.

```Javascript
const date1 = new Date("2021-01-01");
const date2 = new Date("2020-12-31");

if (date1.getTime() > date2.getTime()) {
  console.log("Date 1 is later than Date 2");
} else if (date1.getTime() < date2.getTime()) {
  console.log("Date 1 is earlier than Date 2");
} else {
  console.log("Date 1 is equal to Date 2");
}
```

## Syvempi sukellus päivämäärävertailuun

Kun vertaillaan päivämääriä, on tärkeää huomata, että aika-alueilla voi olla vaikutusta vertailuun. Esimerkiksi kaksi päivämäärää, jotka tapahtuvat samana päivänä, mutta eri aikaan, voidaan katsoa erilaisiksi päivämääriksi.

Lisäksi joskus päivämäärävertailu voi olla monimutkaisempaa, esimerkiksi jos haluat tarkistaa, onko kyseessä karkausvuosi tai onko päivämäärä syntymäpäivä.

Tässä on yksinkertainen esimerkki, joka tarkistaa, onko annettu päivämäärä syntymäpäivä vai ei:

```Javascript
const today = new Date();
const birthday = new Date("2000-01-01");
const birthYear = birthday.getFullYear();

if ((today.getTime() - birthday.getTime()) % 31536000000 === 0) {
  console.log("This year is your birthday!");
} else {
  console.log(`Next year, you will turn ${today.getFullYear() - birthYear + 1} years old.`);
}
```

Tämä esimerkki käyttää `.getFullYear()`-metodia saadakseen vuodet ja modulo-operaattoria tarkistaakseen, onko päivämäärä syntymäpäivä vai ei.

## Katso myös

* [MDN Date](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
* [W3Schools - Javascript Date](https://www.w3schools.com/js/js_dates.asp)
* [Javascript Date Object Explained](https://www.javascripttutorial.net/javascript-date/)