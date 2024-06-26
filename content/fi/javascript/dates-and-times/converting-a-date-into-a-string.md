---
date: 2024-01-20 17:37:19.223982-07:00
description: "How to: Ennen ECMAScriptin (JavaScriptin virallisen standardin) syntymist\xE4\
  , p\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi oli usein selainkohtaista\
  \ ja hankalaa\u2026"
lastmod: '2024-04-05T21:53:58.539579-06:00'
model: gpt-4-1106-preview
summary: "Ennen ECMAScriptin (JavaScriptin virallisen standardin) syntymist\xE4, p\xE4\
  iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi oli usein selainkohtaista ja hankalaa\
  \ yhten\xE4ist\xE4\xE4."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

## How to:
```Javascript
// Luodaan nykyhetken kuvaava Date-objekti
const now = new Date();

// Muunto merkkijonoksi toLocaleDateString()-metodilla (suomalainen formaatti)
console.log(now.toLocaleDateString('fi-FI')); // Esim. tulostus: "24.2.2023"

// Muunto merkkijonoksi toISOString()-metodilla (ISO 8601 formaatti)
console.log(now.toISOString()); // Esim. tulostus: "2023-02-24T12:34:56.789Z"

// Muunto merkkijonoksi JSON-muodossa
console.log(JSON.stringify(now)); // Esim. tulostus: ""2023-02-24T12:34:56.789Z""
```

## Deep Dive
Ennen ECMAScriptin (JavaScriptin virallisen standardin) syntymistä, päivämäärän muuntaminen merkkijonoksi oli usein selainkohtaista ja hankalaa yhtenäistää. ECMAScript standardisoi Date-objektin, mukaan lukien sen merkkijonomuotoilut.

Metodivaihtoehtoja on useita. `Date.prototype.toString()` palauttaa päivämäärän selkeässä, mutta epävirallisessa formaatissa, kun taas `Date.prototype.toISOString()` antaa universaalin ja vertailukelpoisen ISO 8601 -muodon. `toLocaleDateString()` mahdollistaa paikallisen aikamuodon käyttämisen, joka voi olla hyödyllinen käyttäjäkohtaisen esitystavan tarjoamisessa.

JavaScript-kirjastoja kuten Moment.js ja date-fns tarjoavat lisää muotoiluvaihtoehtoja ja käytännöllisyyttä, erityisesti kansainvälisten sovellusten kehityksessä.

## See Also
- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns library](https://date-fns.org/)
- [Moment.js library](https://momentjs.com/)
