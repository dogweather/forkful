---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstin hakeminen ja korvaaminen on prosessi, jossa ohjelma löytää tietyn merkkijonon (tekstin) ja vaihtaa sen toiseen. Ohjelmoijat tekevät tämän tekstin ja datan manipulointiin, usein virheiden korjaamiseksi tai tiedon muuttamiseksi.

## Miten:

Javascriptissa voit käyttää metodit `.replace()` ja `.search()` tekstin etsimiseen ja korvaamiseen. 


```javascript
let str = "Hei maailma!";
let result = str.replace("maailma", "Javascript");

console.log(result); //"Hei Javascript!"
```
Tässä esimerkissä `.replace()` metodi etsii merkkijonoa "maailma" ja korvaa sen merkkijonolla "Javascript".

## Deep Dive

Tekstin etsimisen ja korvaamisen mahdollisuus tuli käytäntöön jo historiallisen ajan tekstinkäsittelyjärjestelmissä, kuten Unixin Sed-komento. Se on pysynyt tärkeänä työkaluna, joka helpottaa ohjelman kehitystyötä.

Vaihtoehtoina `.replace()` metodille on muitakin tapoja tehdä samanlaisia toimintoja, kuten regular expression eli RegEx. RegEx on voimakas ohjelmointikielen työkalu, joka antaa hienostuneemmat tekstin etsintä- ja korvausmöglichkeiten.

```javascript
let str = "Hei maailma!";
let regex = /maailma/gi;
let result = str.replace(regex, "Javascript");

console.log(result); //"Hei Javascript!"
```
Tässä esimerkissä RegEx korvaa kaikki "maailma" esiintymät, riippumatta kirjoitustavasta.

## See Also 

Lisätietoja JavaScriptin tekstin etsinnästä ja korvaamisesta voit löytää seuraavilta sivustoilta:
- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3Schools: JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [Eloquent JavaScript: Regular Expressions](https://eloquentjavascript.net/09_regexp.html)