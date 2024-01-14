---
title:                "TypeScript: Sattumanvaraisten numeroiden luominen"
programming_language: "TypeScript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi
Monissa JavaScript-projekteissa tarvitaan satunnaislukujen generointia erilaisiin tarkoituksiin, kuten pelimekaniikkaan, tietojen analysointiin tai yksinkertaisesti satunnaisen sisällön tuottamiseen. TypeScriptin avulla tämä on helppoa ja nopeaa toteuttaa.

## Kuinka Tehdä
Satunnaislukujen generoiminen TypeScriptissä onnistuu käyttämällä `Math.random()` -funktiota, joka palauttaa aina luvun väliltä 0-1. Tämä luku kerrotaan halutulla alueella ja pyöristetään, jotta saadaan haluttu lukuväli. Esimerkiksi, jos haluamme generoida kokonaislukuja väliltä 1-10, voimme käyttää seuraavaa koodia:

```TypeScript
const randomNumber: number = Math.round(Math.random() * 9 + 1);
console.log(randomNumber); // Tulos voi olla esimerkiksi 4
```

## Syvempi Sukellus
Satunnaislukujen generoiminen perustuu algoritmeihin, joita käytetään luvun laskemiseen ja jakamiseen. Vaikka `Math.random()` käyttääkin monimutkaisia algoritmeja, se ei kuitenkaan pysty tuottamaan täysin satunnaista lukua. Tässä tapauksessa usein käytetään ulkoisia tekijöitä, kuten ajan funktionaalista riippuvuutta, paremman satunnaisuuden saavuttamiseksi.

## Katso myös
- [Math.random() dokumentaatio](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [JavaScript Random Numbers](https://www.w3schools.com/js/js_random.asp)
- [Generating random numbers in TypeScript](https://www.typescriptlang.org/docs/handbook/2/type-system.html#example-1)