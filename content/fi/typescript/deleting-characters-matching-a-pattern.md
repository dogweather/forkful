---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Poistaminen merkkejä, jotka vastaavat tiettyä mallia, tarkoittaa sen merkkijonon osien etsimistä, joka vastaa tiettyä säännöllistä lauseketta, ja sen poistamista. Ohjelmoijat tekevät tämän usein merkkijonon muotoilun tai epätarkkuuden poistamiseksi tai merkkijonon sisällön suodattamisen yksinkertaistamiseksi.

## Kuinka:

Alla on esimerkkejä siitä, miten voit toteuttaa tämän TypeScriptilla:

```TypeScript
let teksti = 'Hei, olen TypeScript ohjelmoija!';
let poista = /TypeScript /g;

let uusiTeksti = teksti.replace(poista, '');

console.log(uusiTeksti); // "Hei, olen ohjelmoija!"
```
Näiden koodirivien suorittaminen poistaa kaikki "TypeScript " tekstit, jolloin saamme "Hei, olen ohjelmoija!" tulosteeksi.

## Syvä Sukellus:

Historiallisessa kontekstissa merkkijonon manipulointi on ollut osa ohjelmointia ohjelmoinnin alusta alkaen. Konsepti on peräisin aikakaudelta, jolloin muisti oli arvokasta ja ohjelmoijien piti hallita tietojen tallennustilaa tarkasti.

Vaihtoehtoina voit myös käyttää kukin kieliominaisuudet, kuten split-funktio tai map-funktio tietyissä tapauksissa.

Implementointitietojen osalta TypeScript käyttää ECMA-262 standardin String.prototype.replace -metodia. Tämä metodi tarkistaa merkkijonon jokaisen osion säännöllisen lausekkeen vastaavuuden osalta ja korvaa vastaavat osiot annetulla merkkijonolla tai paluuarvolla.

## Katso Myös:

- [MDN dokumentaatio String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace) 
- [TypeScriptin viralliset dokumentit](https://www.typescriptlang.org/docs/handbook/basic-types.html#string) 
- [ECMA-262 standard](https://www.ecma-international.org/publications/standards/Ecma-262.htm)