---
title:                "Alimerkkijonojen erottelu"
html_title:           "TypeScript: Alimerkkijonojen erottelu"
simple_title:         "Alimerkkijonojen erottelu"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Substringien erottaminen tarkoittaa tietyistä merkkijonoista toisten osien poistamista. Ohjelmoijat käyttävät tätä tekniikkaa usein käsitellessään merkkijonoihin liittyviä operaatioita, kuten hakua, käsittelyä ja järjestämistä. Se voi myös auttaa tietojen muotoilussa ja käyttäjän syötteen validoinnissa.

## Miten:
Tässä on muutamia esimerkkejä siitä, miten voit erottaa substringejä TypeScriptissä:

```TypeScript
// Erota merkkijonon alkuosa
const sana = "Hei maailma!";
const alkuosa = sana.substring(0, 3);
console.log(alkuosa); // "Hei"

// Erota merkkijonon loppuosa
const sana = "Hei maailma!";
const loppuosa = sana.substring(4);
console.log(loppuosa); // "maailma"

// Erota merkkijonosta tietty määrä merkkejä
const sana = "Hei maailma!";
const osa = sana.substring(0,8);
console.log(osa); // "Hei maai"
```

## Syvemmälle:
Substringien erottaminen on ollut osa ohjelmointia jo pitkään ja sitä käytetään edelleen monilla eri ohjelmointikielillä. Monet ohjelmoijat käyttävät myös erilaisia ​​algoritmeja ja funktioita, kuten "slice" tai "substring", jotka toimivat samalla periaatteella.

## Lisätietoa:
- [MDN web docs - substring](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [W3Schools - JavaScript string slices](https://www.w3schools.com/js/js_string_slices.asp)