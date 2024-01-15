---
title:                "Kahden päivämäärän vertailu"
html_title:           "TypeScript: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi
Vertailemalla kahta päivämäärää voit tarkistaa niiden suhteellisen sijainnin tai selvittää, onko jokin päivämäärä ennen vai jälkeen toisen.

## Miten
Voit käyttää TypeScriptiä vertailemaan kahta päivämäärää helposti `Date` -luokan `getTime()` -metodin avulla. Se palauttaa millisekuntien kokonaismäärän, joka edustaa päivämäärän ja ajan välistä aikaa. Voit sitten verrata näitä numeroita ja tarkistaa, onko päivämäärä ennen vai jälkeen toisen. Esimerkiksi:

```TypeScript
const date1 = new Date(2020, 5, 10);
const date2 = new Date(2020, 5, 15);

const time1 = date1.getTime();
const time2 = date2.getTime();

if (time1 < time2) {
    console.log("Date 1 is before Date 2");
} else if (time1 > time2) {
    console.log("Date 1 is after Date 2");
} else {
    console.log("Dates are equal");
}
```

Tämä tulostaisi `Date 1 is before Date 2`.

## Syväsukellus
Vaikka `Date` -luokka tarjoaa joitakin hyödyllisiä metodeja, kuten `getTime()` ja `getDate()`, voit myös käyttää `getFullYear()` ja `getMonth()` vertaillaksesi tarkemmin päivämäärän vuodenaikaa tai kuukautta. Näiden metodien avulla voit myös tarkistaa, onko kaksi päivämäärää samassa kuussa tai vuodessa. Esimerkiksi:

```TypeScript
const date1 = new Date(2020, 3, 10);
const date2 = new Date(2020, 5, 10);

if (date1.getFullYear() === date2.getFullYear()) {
    console.log("Same year");
    if (date1.getMonth() === date2.getMonth()) {
        console.log("Same month");
    } else {
        console.log("Different month");
    }
} else {
    console.log("Different year");
}
```

Tämä tulostaisi `Same year` ja `Different month`.

## Katso myös
- [MDN - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Date API](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#handling-dates)