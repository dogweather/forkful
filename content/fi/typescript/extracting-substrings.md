---
title:    "TypeScript: Alalinjanojen erottaminen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: Miksi käyttäisimme TypeScriptillä alimerkkijonojen poimimista?

Alimerkkijonojen poimimisella on monia käyttötarkoituksia, kuten tekstien analysointi, datakäsittely ja merkkijonojen manipulointi. TypeScriptillä alimerkkijonojen poimiminen on helppoa ja tehokasta, ja se voi säästää paljon aikaa ja vaivaa.

## Miten: Alimerkkijonojen poimimisen toteuttaminen TypeScriptillä

Alimerkkijonojen poimiminen TypeScriptillä onnistuu helposti käyttämällä sisäänrakennettuja merkkijonojen käsittelytoimintoja, kuten `substring()` ja `slice()`. Voit aloittaa tallentamalla alkuperäisen merkkijonon muuttujaan, jotta voit helposti käsitellä sitä jatkossa. Alla on esimerkkikoodi, jossa poimimme alimerkkijonon ja tulostamme sen konsoliin:

```TypeScript
// Alustetaan muuttuja alkuperäisellä merkkijonolla
let alkuperainenMerkkijono = "Tämä on esimerkkiteksti";

// Poimitaan alimerkkijono indekseistä 5-11
let alimerkkijono = alkuperainenMerkkijono.substring(5, 11);

// Tulostetaan alimerkkijono konsoliin
console.log(alimerkkijono); // t on e
```

Yllä oleva koodi tulostaa konsoliin alimerkkijonon "t on e", joka on alkuperäisen merkkijonon 5.-11. kirjain.

## Syvempi sukellus: Alimerkkijonojen poimiminen TypeScriptillä

Alimerkkijonojen poimiminen TypeScriptillä perustuu merkkijonojen sisäänrakennettuihin käsittelytoimintoihin, kuten `substring()` ja `slice()`. Nämä toiminnot ottavat vastaan kaksi parametria: aloitusindeksin ja lopetusindeksin. Aloitusindeksi määrittää, mistä kohdasta alkaen alimerkkijono poimitaan, ja lopetusindeksi määrittää, mihin kohtaan saakka alimerkkijono poimitaan. Sekä `substring()` että `slice()` antavat saman lopputuloksen, mutta niiden toiminta eroaa hieman, kun indeksit ovat negatiivisia.

Näitä toimintoja voi myös ketjuttaa, jolloin voit poimia useampia alimerkkijonoja yhdestä alkuperäisestä merkkijonosta.

## Katso myös

- [TypeScriptin merkkijonojen käsittelytoiminnot](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [substring() vs slice()](https://www.geeksforgeeks.org/javascript-string-substring-vs-slice/)
- [Merkkijonojen käsittelystä TypeScriptissä](https://itnext.io/manipulating-strings-in-typescript-207e91db2527)