---
date: 2024-01-20 17:46:47.381956-07:00
description: "How to: (Kuinka tehd\xE4:) Alun perin substring-toimintoja tarvittiin\
  \ tiedon k\xE4sittelyyn: tiedostopoluista nimen erottaminen, k\xE4ytt\xE4j\xE4nimist\xE4\
  \ domain-erottelu\u2026"
lastmod: '2024-04-05T22:51:10.455029-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Alun perin substring-toimintoja tarvittiin tiedon k\xE4\
  sittelyyn."
title: Merkkijonojen osien poimiminen
weight: 6
---

## How to: (Kuinka tehdä:)
```TypeScript
// Otetaan esimerkki TypeScriptillä
let lause: string = "Hello, maailma!";

// substring-metodi (alkuindeksi, loppuindeksi):
let osajono1 = lause.substring(7, 14);
console.log(osajono1); // tulostuu "maailma"

// slice-metodi (alkuindeksi, loppuindeksi):
let osajono2 = lause.slice(7, -1);
console.log(osajono2); // tulostuu "maailma!"

// substr-metodi (alkuindeksi, pituus):
// Huom: vanhentunut, mutta esimerkin vuoksi tässä
let osajono3 = lause.substr(7, 7);
console.log(osajono3); // tulostuu "maailma"
```

## Deep Dive (Sukellus syvyyksiin)
Alun perin substring-toimintoja tarvittiin tiedon käsittelyyn: tiedostopoluista nimen erottaminen, käyttäjänimistä domain-erottelu jne. Historiallisesti näitä on hyödynnetty monissa ohjelmointikielissä, kuten JavaScriptissä, josta TypeScript on ponnistanut.

JavaScriptissä substringit ovat olleet pitkään käytössä, ja TypeScript on luontevasti tuonut mukanaan tämän perinnön. `substring`, `slice` ja `substr` ovat kaikki eri tapoja saada aikaan sama lopputulos, mutta on hyvä huomata, että `substr`-metodi on vanhentunut ja sitä tulisi välttää uudessa koodissa.

`substring` ja `slice`-metodeilla on pieniä eroavaisuuksia käytössä: esim. `substring` ei salli negatiivisia indeksejä, kun taas `slice` käsittelee ne älykkäästi osoittamaan merkkijonon lopusta. Valinta riippuu tilanteesta ja tarpeista, mutta `slice` on monipuolisempi.

## See Also (Katso myös)
- [MDN Web Docs - String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs - String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
