---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonon yhdistäminen tarkoittaa kahden tai useamman merkkijonon yhdistämistä yhdeksi merkkijonoksi. Ohjelmoijat tekevät tämän tietojen muodostamiseksi tai tietojen käsittelyn yksinkertaistamiseksi.

## Kuinka:

Tässä on kaksi tapaa yhdistää merkkijonoja Javascriptin uusimmissa versioissa:

```Javascript
let tervehdys = "Hei, ";
let nimi = "Matti!";
let viesti = tervehdys.concat(nimi);
console.log(viesti); // Tulostaa: "Hei, Matti!"
```

Tai ES6:ssa template literaalien avulla:

```Javascript
let tervehdys = "Hei, ";
let nimi = "Matti!";
let viesti = `${tervehdys}${nimi}`;
console.log(viesti); // Tulostaa: "Hei, Matti!"
```

## Sukellus syvemmälle:

Historiallinen konteksti: Alkuperäinen `concat`-metodi on ollut mukana Javascriptissä alusta alkaen.

Vaihtoehdot: `+` -operaattoria, `concat`-metodia ja  ES6:n Template literaaleja voidaan käyttää merkkijonojen yhdistämiseen.

Toteutusyksityiskohdat: Silloin kun käytetään useita `+` -operaattoreita, suorituskyky voi heikentyä, koska jokainen `+` luo uuden merkkijonon. Tämä voi johtaa lisääntyneeseen muistin kulutukseen ja hitaampaan suoritukseen. Toisaalta, template literaalit ja `concat`-metodi eivät luo uutta merkkijonoa jokaisella yhdistämisellä.

## Katso Myös:

1. [MDN: String.prototype.concat()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
2. [MDN: Template literaalit](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Template_literals)
3. [JavaScript Concatenation Performance](https://jsperf.com/js-string-concatenation/3)