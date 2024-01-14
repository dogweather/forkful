---
title:    "TypeScript: Mallia vastaavien merkkien poistaminen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi poistaa merkkejä, jotka vastaavat kaavaa?

Merkkien poistaminen, jotka vastaavat tiettyä kaavaa, voi olla hyödyllistä, jos haluat puhdistaa tietoa tai tarkistaa sen tietyn muodon tai formaatin. Tämä voi auttaa sinua varmistamaan, että tiedot ovat oikein ja helposti käsiteltävissä myöhemmin.

## Kuinka tehdä se TypeScriptillä?

Voit käyttää TypeScriptiä poistaaksesi merkkejä, jotka vastaavat tiettyä kaavaa, käyttämällä `replace`-metodia ja säännöllisiä lausekkeita.

Esimerkiksi, jos haluat poistaa kaikki välilyönnit merkkijonosta, voit käyttää seuraavaa koodia:

```TypeScript
const merkkijono = "Tämä on esimerkki merkkijonosta.";
console.log(merkkijono.replace(/\s/g, "")); // Output: Tämäonesimerkkimerkkijonosta.
```

Lopullisessa koodissa `/ \s/g`-osa edustaa säännöllistä lauseketta, joka etsii kaikki välilyönnit ja korvaa ne tyhjällä merkkijonolla `""`.

Toinen esimerkki voisi olla uuden puhelinnumeron muotoilu oikeaan muotoon poistamalla väliviivat ja sulkumerkit:

```TypeScript
const puhelinnumero = "(123) 456-7890";
console.log(puhelinnumero.replace(/[\(\)\-\s]/g, "")); // Output: 1234567890
```

Lopputuloksena säännöllisen lausekkeen `/[\(\)\-\s]/g` käyttämisestä on, että kaikki sulkumerkit, väliviivat ja välilyönnit poistetaan.

## Syvällinen sukellus

Kun poistat merkkejä, jotka vastaavat tiettyä kaavaa, on tärkeää ymmärtää säännöllisten lausekkeiden erilaiset merkitykset ja käyttötavat. Voit muuttaa / korvata vaihtelevia merkkejä, sijoittaa säännöllisiä lausekkeita muihin lausekkeisiin ja paljon muuta.

Joten, jos haluat hallita merkkien poistamista tietystä kaavasta, suosittelen tutustumaan säännöllisiin lausekkeisiin ja niiden erilaisiin mahdollisuuksiin.

## Katso myös

- [Säännöllisten lausekkeiden opas (MDN)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Säännölliset lausekkeet TypeScriptissä (TypeScript-kirjasto)](https://github.com/microsoft/TypeScript/wiki/Regular-Expressions)