---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkinjonojen interpolointi tarkoittaa muuttujien ja muiden arvojen sijoittamista tekstimuotoiseen merkkijonoon. Ohjelmoijat käyttävät tätä säilyttämään koodin selkeänä ja ylläpidettävänä sekä dynaamisesti muuttuvien arvojen käsittelyyn.

## Näin se toimii:
Katsotaan miten merkkijonojen interpolointi tehdään Javascriptillä.

```Javascript
let nimi = "Pekka";
let tervehdys = `Hei, ${nimi}!`;
console.log(tervehdys); //Output: "Hei, Pekka!"
```
Tässä yllä olevassa koodissa, `${nimi}` sijoittaa muuttujan `nimi` arvon merkkijonoon.

## Syvällisemmin
Interpolointi tuli suosituksi muiden ohjelmointikielten, kuten Ruby ja Perl, kanssa. JavaScriptissä tämä lisättiin osaksi ECMAScript 2015 -standardia.

Vaihtoehtoisia tapoja ovat 
1. Merkkijonon yhdistäminen käyttämällä `+` -operaattoria:
   ```Javascript
   let nimi = "Pekka";
   let tervehdys = "Hei, " + nimi + "!";
   console.log(tervehdys); //"Hei, Pekka!"
   ```
 
2. `concat()` -funktion käyttö:
   ```Javascript
   let nimi = "Pekka";
   let tervehdys = "Hei, ".concat(nimi,"!");
   console.log(tervehdys); //"Hei, Pekka!"
   ```
 
Interpoloituja merkkijonoja ei voida toteuttaa vanhemmissa selaimissa, jotka eivät tue ECMAScript 2015 -standardia tai uudempaa.

## Katso myös 
MDN String Interpolation: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals

ECMAScript 2015 (ES6) standardi: https://262.ecma-international.org/6.0/ 

JavaScript String concat() Method: https://www.w3schools.com/jsref/jsref_concat_string.asp