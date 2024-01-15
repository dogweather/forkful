---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Javascript: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi
Miksi haluaisit yhdistellä merkkijonoja? Yksinkertaisesti sanottuna, se on yksi tapa luoda joustavia ja muokattavissa olevia viestejä, tekstejä ja laskelmia Javascript-ohjelmoinnissa.

## Miten
Concatenating eli merkkijonojen yhdistely Javascriptissä on helppoa! Käytä vain plus-merkkiä (+) yhdistääksesi haluamasi merkkijonot. Katso alla oleva koodiesimerkki ja sen tuottama tulos.

```Javascript
var firstName = "Anna";
var lastName = "Korhonen";

// Yhdistetään merkkijonot
var fullName = firstName + " " + lastName;
console.log(fullName);

// Tulostaa "Anna Korhonen"
```

## Deep Dive
Merkkijonojen yhdistelyä käytetään usein luodessa dynaamisia viestejä, jotka sisältävät muuttujia tai käyttäjän antamia tietoja. Merkkijonojen yhdistelemisen sijaan voit myös käyttää `template literal` -syntaksia (` `) paljon monimutkaisempien ja tehokkaampien merkkijonojen luomiseen. Voit lukea lisää tästä aiheesta [täältä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals).

## Katso myös
- [MDN: Merkkijonon yhdisteleminen](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition_assignment)
- [W3Schools: Merkkijonon yhdisteleminen](https://www.w3schools.com/js/js_string_concat.asp)