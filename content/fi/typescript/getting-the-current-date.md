---
title:    "TypeScript: Nykyisen päivämäärän noutaminen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi?

Jokaisen sovelluksen tarvitsee joskus käyttää päivämäärää joko käyttäjän näyttämiseksi tai tietojen tallentamiseen tietokantaan. Päivämäärän saaminen nykyisestä ajasta on yksi perustavanlaatuisista toiminnoista, joita jokaisen ohjelmoijan tulee tietää.

## Miten?

Päivämäärän saaminen nykyisestä ajasta on yksinkertaista TypeScript-kielellä. Käyttämällä sisäänrakennettua `Date`-luokkaa, voimme käyttää sen `getDate()`, `getMonth()` ja `getFullYear()` -metodeja palauttamaan halutun ajanhetken.

```TypeScript
const currentDate: Date = new Date();

console.log(`Nykyinen päivä: ${currentDate.getDate()}`);
console.log(`Nykyinen kuukausi: ${currentDate.getMonth() + 1}`); // Muista lisätä yksi, koska kuukaudet alkavat nollasta
console.log(`Nykyinen vuosi: ${currentDate.getFullYear()}`);
```

Koodin tuloste:

```
Nykyinen päivä: 7
Nykyinen kuukausi: 10
Nykyinen vuosi: 2021
```

## Syvemmälle

Date-luokka tarjoaa myös muita hyödyllisiä metodeja kuten `getHours()`, `getMinutes()` ja `getSeconds()` jos haluat myös saada tarkempia tietoja nykyisestä ajasta. Lisäksi `Date`-luokkaan voidaan antaa myös parametrejä, kuten vuosi, kuukausi ja päivä, jolloin se palauttaa tietyn päivämäärän tiedot.

## Katso myös

- [TypeScript Date-luokan dokumentaatio](https://www.typescriptlang.org/docs/handbook/date-and-time.html)
- [W3Schools - JavaScript Date-objekti](https://www.w3schools.com/js/js_dates.asp)
- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)