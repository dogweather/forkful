---
title:                "Javascript: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi: Merkkijonojen yhdistäminen
Miksi joku ylipäänsä haluaisi käyttää aikaa ja vaivaa merkkijonojen yhdistämiseen? Yksinkertaisesti sanottuna, merkkijonojen yhdistäminen on tärkeä osa Javascript-ohjelmointia, joka mahdollistaa erilaisten tekstipohjaisten tietojen yhdistämisen ja käytön.

## Miten: Esimerkkejä ja koodilohkoja
Merkkijonojen yhdistämistä varten käytetään plus-merkkiä (`+`) tai `.concat()` -funktiota. Seuraavassa on esimerkki:

```Javascript
let etunimi = "Matti";
let sukunimi = "Meikäläinen";

// Plus-merkin käyttö:
let nimi = etunimi + " " + sukunimi;
console.log(nimi); //OUTPUT: "Matti Meikäläinen"

// .concat()-funktion käyttö:
let nimi = etunimi.concat(" ", sukunimi);
console.log(nimi); //OUTPUT: "Matti Meikäläinen"
```

## Syvällisempi tarkastelu: Merkkijonojen yhdistämisen takana
Merkkijonojen yhdistäminen toimii periaatteessa siten, että se "liimaa" kaksi merkkijonoa yhteen. Tätä käytetään esimerkiksi silloin, kun halutaan luoda kokonaisia lauseita tai yhdistää muuttujien arvoja. Plus-merkin käytössä täytyy huomioida, että jos yhdistettävänä on eri tyyppisiä muuttujia, ne muunnetaan aina merkkijonoiksi. Myös `.concat()`-funktio toimii samalla periaatteella, mutta se mahdollistaa enemmän merkkijonojen yhdistelyä ketjujen muodossa.

## Katso myös
- [MDN: String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [w3schools: JavaScript Strings](https://www.w3schools.com/js/js_strings.asp)
- [Codecademy: Strings in Javascript](https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-strings)