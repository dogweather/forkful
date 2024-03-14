---
date: 2024-01-26 03:46:05.694752-07:00
description: "Py\xF6rist\xE4minen on ylim\xE4\xE4r\xE4isen desimaalikohinan poistoa\
  \ luvusta tietyn pisteen j\xE4lkeen. Ohjelmoijat py\xF6rist\xE4v\xE4t hallitakseen\
  \ tarkkuutta, hallinnoidakseen\u2026"
lastmod: '2024-03-13T22:44:56.944739-06:00'
model: gpt-4-0125-preview
summary: "Py\xF6rist\xE4minen on ylim\xE4\xE4r\xE4isen desimaalikohinan poistoa luvusta\
  \ tietyn pisteen j\xE4lkeen. Ohjelmoijat py\xF6rist\xE4v\xE4t hallitakseen tarkkuutta,\
  \ hallinnoidakseen\u2026"
title: "Numerojen py\xF6rist\xE4minen"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Pyöristäminen on ylimääräisen desimaalikohinan poistoa luvusta tietyn pisteen jälkeen. Ohjelmoijat pyöristävät hallitakseen tarkkuutta, hallinnoidakseen muistia tai tehdäkseen tulosteesta käyttäjäystävällisen – esimerkiksi muuttamalla 2.998 puhtaaksi 3:ksi.

## Miten:
Tässä on, miten voit pyöristää numeroita JavaScriptissä käyttämällä `Math.round()`, `Math.ceil()`, ja `Math.floor()`:

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (koska .567 on enemmän kuin .5)

console.log(roundedDown); // Tulostaa: 2
console.log(roundedUp);   // Tulostaa: 3
console.log(rounded);     // Tulostaa: 3
```

Kiinnittääksesi tiettyyn määrään desimaalipaikkoja, käytä `toFixed()`:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (palauttaa merkkijonon)

console.log(twoDecimals); // Tulostaa: "2.57"
```

Muunna merkkijono takaisin numeroksi unariaalisen plussan tai `Number()` avulla:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // Tulostaa: 2.57
```

## Syväsukellus
Numeroiden pyöristäminen ei ole uutta; se on yhtä vanhaa kuin numerot itse. JavaScriptissä `Math.round()` käyttää "pyöristä puoli ylöspäin" -tasausmenetelmää: jos fraktionaalinen osa on 0.5, se pyöristää lähimpään parilliseen numeroon.

Saat enemmän kontrollia, `toFixed()` voi olla sinun valintasi, mutta muista, se palauttaa merkkijonon. Takaisin numeroksi muuntaminen voi olla ylimääräinen askel, mutta varmistaa, että jatkat työskentelyä numeeristen tyyppien kanssa.

Vaihtoehtoja? Kirjastot kuten `lodash` tarjoavat `_.round(number, [precision=0])` tarkemman kontrollin saamiseksi. Tai, uudempi `Intl.NumberFormat` antaa sinulle korkean tarkkuuden muotoilun pelkän pyöristämisen sijaan.

Puhuttaessa tarkkuudesta, varo JavaScriptin liukulukujen oikkuja. `0.1 + 0.2` ei täsmälleen yhtä kuin `0.3` johtuen siitä, miten numerot tallennetaan. Joskus pyöristäminen on tarpeen korjatakseen tällaiset liukulukuvirheet.

## Katso Myös
- Mozillan Matematiikkadokumentaatio: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Taloudellinen pyöristäminen `Intl.NumberFormat` avulla: [ECMAScript Internationalization API](https://tc39.es/ecma402/#numberformat-objects)
- `lodash` pyöristäminen: [Lodash Docs](https://lodash.com/docs/4.17.15#round)
