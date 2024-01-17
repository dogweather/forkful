---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "TypeScript: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonon pituuden löytäminen on yksi yleisimmistä tehtävistä ohjelmoinnissa. Se tarkoittaa yksinkertaisesti merkkijonon sisältämien merkkien kokonaismäärän laskemista. Tämä on tärkeää monissa ohjelmointitehtävissä, kuten tietokantojen hakutoiminnoissa ja datan analysoinnissa.

## Näin teet sen:
```TypeScript
const sana = "Hei!"
console.log(sana.length);
```
Tämä koodinpätkä tulostaa konsoliin numeron 4, koska "Hei!"-merkkijonossa on neljä merkkiä.

## Syväsukellus:
Merkkijonojen pituuden laskeminen on ollut osa ohjelmointia jo pitkään. Alun perin tämä oli hieman monimutkaisempaa, sillä merkkijonot tallennettiin muistissa tiiviinä merkkijonomuuttujina, joihin piti käydä läpi symboli kerrallaan. Nykyisin tämä hoituu kätevästi valmiilla .length -ominaisuudella.

On myös olemassa muita tapoja laskea merkkijonon pituus, kuten käyttämällä looppeja ja muuttujia. Tällainen lähestymistapa voi olla hyödyllinen, jos haluat esimerkiksi tarkistaa merkkijonon sisältämien tiettyjen merkkien määrän.

## Katso myös:
- [Mozilla Developer Network: String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Stack Overflow: How do you get the string length in TypeScript?](https://stackoverflow.com/questions/48567168/how-do-you-get-the-string-length-in-typescript)