---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Sidan pituus: Mitä ja Miksi?

Jousen pituuden selvittäminen tarkoittaa sen merkkien lukumäärän laskemista. Tätä tietoa tarvitaan esimerkiksi silloin, kun halutaan tietää, sisältääkö merkkijono tietyn määrän merkkejä tai kun halutaan käydä läpi merkkijonon jokainen merkki.

## Näin se tehdään:

Tämä on kuinka voit selvittää merkkijonon pituuden JavaScriptillä:

 ```Javascript...
let str = "Hei maailma!";
let length = str.length;
console.log(length); // tulostaa: 12
``` 

Joten, meillä on merkkijono "Hei maailma!", jonka pituus on 12 merkkiä (sisältäen huutomerkki).

## Syvempi sukellus

Sadan pituuden selvittämisen JavaScriptissä on melko suoraviivaista, kuten olemme nähneet. Mutta onko olemassa muita tapoja?

Syvämmällä tasolla JavaScript tallentaa merkkijonot UTF-16-koodauksena, jossa jokainen merkki vie joko kaksi tai neljä tavua tilaa. Tämä tarkoittaa, että jos kyseessä on =unicode merkkijono, .length ei välttämättä palauta odotettua tulosta. Silloin on käytettävä toista toimintoa, kuten Array.from(str).length .

Historiallisesti, ennen ECMAScript 2015, merkkijonon pituuden selvittämiseksi täytyi käydä läpi koko merkkijono käyttäen silmukkaa ja kasvattaa laskuria jokaisella kierroksella. 

## Katso myös

1. [MDN Web Docs: String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
2. [ECMAScript 2015 specs](http://www.ecma-international.org/ecma-262/6.0/#sec-properties-of-string-instances-length)
3. [JavaScript Info: String](https://javascript.info/string)