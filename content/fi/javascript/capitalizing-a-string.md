---
title:                "Merkkijonon isot kirjaimet"
html_title:           "Javascript: Merkkijonon isot kirjaimet"
simple_title:         "Merkkijonon isot kirjaimet"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi 

Joskus haluat muuttaa merkkijonon kirjainkoon, jotta se olisi helpompi lukea tai vastaisi tiettyä muotoilusääntöä. Tämän artikkelin avulla opit, kuinka voit käyttää muutamia yksinkertaisia Javascript-funktioita saadaksesi tämän aikaan.

## Kuinka tehdä

```Javascript
function capitalize(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize("javascript")) // Tulostaa "Javascript"
```

Tämä esimerkki käyttää `charAt()` ja `toUpperCase()` -funktioita muuttaakseen merkkijonon ensimmäisen kirjaimen isoksi ja `slice()`-funktiota lisätäkseen sen takaisin alkuperäisen merkkijonon perään. Tämä on yksinkertainen tapa muuttaa merkkijonon ensimmäinen kirjain isoksi. Voit myös käyttää muita funktioita, kuten `split()` ja `join()` ensimmäisen kirjaimen muuttamiseen, mutta tämä on yksi helpoimmista tavoista.

```Javascript
String.prototype.capitalize = function() {
  return this.charAt(0).toUpperCase() + this.slice(1);
}

console.log("javascript".capitalize()) // Tulostaa "Javascript"
```

Tämä toinen esimerkki käyttää merkkijonon prototyyppiä mahdollistaakseen `capitalize()`-kutsun suoraan merkkijonoon. Tämä voi olla hyödyllistä, jos haluat käyttää `capitalize()`-funktiota useammin koodissasi.

## Syvä sukellus

On myös mahdollista, että haluat vain muuttaa merkkijonon ensimmäistä kirjainta tietyllä alueella. Tässä tapauksessa voit käyttää `substring()`-funktiota muuttaaksesi osan merkkijonosta ja sitten yhdistää tämän osan muun merkkijonon kanssa.

```Javascript
function capitalizeSentence(sentence) {
  var firstChar = sentence.substring(0, 1).toUpperCase();
  var restOfSentence = sentence.substring(1);
  return firstChar + restOfSentence;
}

console.log(capitalizeSentence("hello world")) // Tulostaa "Hello world"
```

Voit myös käyttää `replace()`-funktiota muuttaaksesi vain tietyn osan merkkijonosta.

```Javascript
function capitalizeWord(word) {
  return word.replace(/^\w/, function (c) {
    return c.toUpperCase();
  });
}

console.log(capitalizeWord("javascript")) // Tulostaa "Javascript"
```

Edellinen esimerkki käyttää säännöllistä lausetta ja `replace()`-funktiota muuttaakseen ensimmäisen merkin isoksi. Tämä voi olla hyödyllistä, jos haluat käyttää `capitalize()`-funktiota vain yhden sanan muuttamiseksi.

## Katso myös

- [MDN - String.prototype.charAt()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [MDN - String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN - String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN - String.prototype.split()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [MDN - String.prototype.join()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/join)
- [MDN - String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)