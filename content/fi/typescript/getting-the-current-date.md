---
title:    "TypeScript: Saat nykyisen päivämäärän"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit hakea nykyisen päivämäärän TypeScriptillä? Olet ehkä rakentamassa sovellusta, joka tarvitsee tietoa ajasta, kuten aikaleimoja tai päivämääriä.

## Miten

Voit käyttää sisäänrakennettua JavaScriptin `Date()` -toimintoa, joka palauttaa nykyisen päivämäärän. Se näyttää UTC-aikaa, mutta voit muuttaa sen paikalliseksi ajaksi käyttämällä `toLocaleDateString()` -funktiota. Katso alla oleva koodiesimerkki nähdäksesi, miten tämä toimii TypeScript-ympäristössä ja tulostus.

```TypeScript
const nykyinenPaivamaara = new Date();
console.log(nykyinenPaivamaara.toLocaleDateString());
```

Tulostus: `11/14/2021`

Voit myös muokata tulostettavan päivämäärän muotoa käyttämällä `toLocaleDateString()` -funktiota ja antamalla haluamasi muoto merkkijonona. Alla olevassa esimerkissä käytämme "FI-fi" -muotoa, joka antaa suomalaiselle päivämäärälle oikean formaatin.

```TypeScript
console.log(nykyinenPaivamaara.toLocaleDateString("FI-fi"));
```

Tulostus: `14.11.2021`

## Syvällinen tarkastelu

`Date()` -toiminto palauttaa aina nykyisen päivämäärän ja ajan millisekunteina UTC-aikana. Millisekunneilla tarkoittaa, että tarkkuus on jopa tuhannesosasekuntien tasolla. Voit myös käyttää muita `Date` -olion metodeja hakeaksesi erilaisia tietoja, kuten kuukausi ja vuosi. Tämä antaa sinulle mahdollisuuden mukauttaa päivämäärää vielä enemmän tarpeidesi mukaan.

## Katso myös

- [MDN web docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript - Date](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [Moment.js](https://momentjs.com/)