---
title:    "Javascript: Tekstin haku ja korvaaminen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Ohjelmoijat käyttävät tekstin etsimistä ja vaihtamista työkaluinaan manipuloidessaan ja muokatessaan tekstejä. Näiden toimintojen avulla voidaan nopeasti ja tarkasti tehdä tiettyjä muutoksia tekstiin, mikä säästää paljon aikaa ja vaivaa.

## Miten

Tekstin etsiminen ja vaihtaminen on yksinkertaista käyttäen Javascriptin sisäänrakennettua metodologiaa `.replace()`. Tämä metodi etsii halutun tekstin ja korvaa sen toisella tekstillä. Esimerkiksi:

```Javascript
let text = "Tervetuloa JavaScript-maailmaan!";
let newText = text.replace("JavaScript", "React");
console.log(newText); 
// tulostaa "Tervetuloa React-maailmaan"
```

Voimme myös etsiä ja vaihtaa useita esiintymiä yhdellä kertaa käyttämällä säännöllisiä lausekkeita:
```Javascript
let text = "Tämä on vain esimerkki. Esimerkki vain.";
let newText = text.replace(/esimerkki/g, "malli");
console.log(newText);
// tulostaa "Tämä on vain malli. Malli vain."
```

## Syvemmälle

Javascriptin `.replace()`-metodi hyödyntää säännöllisiä lausekkeita ja erityisiä merkintöjä etsiessään ja vaihtaessaan tekstiä. Voimme esimerkiksi käyttää "i"-merkintää löytääksemme tekstin tapauksesta riippumatta tai "g"-merkintää vaihtaaksemme kaikki esiintymät. Voimme myös antaa funktiolle parametrina toisen funktion, joka mahdollistaa monimutkaisemman käsittelyn tekstille.

Kannattaa myös huomata, että `.replace()`-metodi ei muuta alkuperäisen tekstin arvoa vaan luo uuden muutetun version. Jos haluamme muuttaa alkuperäistä tekstiä, voimme tallentaa uuden version alkuperäisen muuttujan päälle.

## Katso myös

- [MDN web docs: String.prototype.replace()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [JavaScript.info: Regular expressions](https://javascript.info/regexp)
- [Codeacademy: Using regular expressions in JavaScript](https://www.codecademy.com/learn/introduction-to-javascript/modules/javascript-regex/cheatsheet)