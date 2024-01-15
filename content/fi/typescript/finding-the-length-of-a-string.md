---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "TypeScript: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Stringin pituuden selvittäminen on tärkeä perustaito jokaiselle TypeScript-kehittäjälle. Se auttaa sinua ymmärtämään paremmin ohjelmointikielen toimintaa ja parantamaan koodisi tehokkuutta.

## Miten

```TypeScript
// Esimerkki 1: Selvitä stringin pituus käyttäen .length-metodia
let teksti = "Tämä on esimerkki";
console.log(teksti.length);
// Output: 19

// Esimerkki 2: Selvitä stringin pituus käyttäen for-silmukkaa
let sana = "koodaus";
let pituus = 0;

for(let i=0; i < sana.length; i++) {
    pituus++;
}

console.log(pituus);
// Output: 7
```

Kuten esimerkeistä nähdään, stringin pituuden selvittäminen on helppoa. Voit käyttää joko .length-metodia tai for-silmukkaa, joka laskee jokaisen merkin yksitellen. Ole kuitenkin varovainen, sillä joissakin tapauksissa .length-metodi saattaa antaa väärän arvon, jos merkkijonassa on erikoismerkkejä.

## Syvempi sukellus

Stringin pituus lasketaan sen sisältämien merkkien määrän perusteella. Merkkejä, kuten space ja enter, lasketaan myös mukaan pituuteen. On myös tärkeä huomata, että stringin pituus ei ole muutettavissa. Jos haluat muuttaa stringin pituutta, joudut luomaan uuden stringin.

## Katso myös

- [MDN web docs - String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [TypeScript Documentation - Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)