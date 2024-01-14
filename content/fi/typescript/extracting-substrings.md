---
title:                "TypeScript: Alimerkkijonojen erottaminen"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erottelu on tärkeä osa ohjelmointia ja se auttaa meitä käsittelemään merkkijonoja tehokkaasti. Esimerkiksi kun haluamme hakea tiettyä tekstipätkää suuremmasta merkkijonosta, voimme käyttää substring-funktiota.

## Miten

Alla on TypeScript-koodeja, jotka näyttävät, miten voimme käyttää substring-funktiota eri tilanteissa. Huomaa, että jokaisen esimerkin jälkeen on esitetty myös tuloste konsolissa.

```TypeScript
// Yksinkertainen käyttö: erottaa substrings alusta alkaen annettuun indeksiin asti
let sana: string = "Hei maailma";
let eritelty = sana.substring(0, 3); // substrings alusta alkaen annettuun indeksiin asti (ei sisälly indeksiin 3)
console.log(eritelty); // Tulostaa "Hei"

// Negatiiviset indeksit: laskee merkkijonon lopusta alkaen
let numerot: string = "12345";
let viimeinen = numerot.substring(-4); // Hakee merkkijonon kolme viimeistä merkkiä
console.log(viimeinen); // Tulostaa "345"
```

```TypeScript
// Etsii tiettyä merkkijonoa ja palauttaa sen alkuperäisen indeksin
let lause: string = "Ohjelmointi on hauskaa";
let indeksi = lause.indexOf("hauskaa"); // Palauttaa 16 (indeksi, jossa sana "hauskaa" alkaa)
let eritelty = lause.substring(indeksi); // Erittää merkkijonon halutusta indeksistä loppuun saakka
console.log(eritelty); // Tulostaa "hauskaa"
```

```TypeScript
// Käyttää regular expressionia hakeakseen tietyn kaavan mukaisia merkkijonoja
let sana: string = "Kello on 12:00";
let oikeinMuotoiltu = sana.substring(sana.search(/\d+:\d+/)); // Hakee merkkijonon, jossa on numeroita kaksoispisteellä erotettuna
console.log(oikeinMuotoiltu); // Tulostaa "12:00"
```

## Syvällinen sukellus

Substring-funktiolla on myös muita ominaisuuksia, kuten sen käyttö eri tietotyypeillä ja miten se käsittelee tyhjiä merkkijonoja. On tärkeää huomioida myös indeksien käsittely, jotta vältetään virheet koodissa. Voit lukea lisää näistä ominaisuuksista TypeScriptin virallisesta dokumentaatiosta.

## Katso myös

- [TypeScriptin virallinen dokumentaatio](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [MDN Web Docs -substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [W3Schools -substring()](https://www.w3schools.com/jsref/jsref_substring.asp)