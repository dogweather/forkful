---
title:    "Javascript: Tulostaminen virheenjäljityslähtöön"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi
Kun kehitämme ohjelmia, debuggaus on tärkeä osa prosessia. Se auttaa löytämään virheitä ja korjaamaan niitä. Tulostaminen debug-tietoa voi auttaa meitä ymmärtämään, mitä koodi tekee ja missä virheet ovat. 

## Miten
JavaScriptissä voimme käyttää `console.log()` funktiota tulostamaan tietoa konsoliin. Tämä on hyödyllinen tapa tarkistaa, mitä ohjelma tekee tietyllä hetkellä. 

```Javascript
// Esimerkki koodista
let a = 5;
let b = 10;
let summa = a + b;
console.log(summa);
```

Tulostus konsoliin olisi `15`, joka kertoo meille, että muuttujien `a` ja `b` arvojen summa tallennetaan muuttujaan `summa`.

Konsolin lisäksi voimme myös tulostaa tietoa suoraan verkkosivulle käyttäen HTML-elementtiä `div`.

```Javascript
// Esimerkki koodista
let teksti = "Tervetuloa!";
document.getElementById("debug").innerHTML = teksti;
```

Tämä tulostaisi `<div>` elementin sisällä olevan tekstin "Tervetuloa!". 

## Syventävä sukellus
Debuggaus on kätevä tapa tarkistaa koodin toimivuutta ja löytää virheitä. Tulostamalla debug-tietoa voimme paikallistaa ongelmalliset kohdat koodissa ja korjata ne nopeasti. On kuitenkin tärkeää muistaa poistaa kaikki debug-tulostukset ennen ohjelman julkaisemista, jotta sivu ei näytä epäammattimaiselta käyttäjille.

## Katso myös
- [MDN - Console API](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [Debugging JavaScript with Console Commands](https://www.digitalocean.com/community/tutorials/how-to-debug-javascript-within-the-browser-console#using-console-methods)
- [JavaScript Debugging Tips and Tricks](https://blog.bitsrc.io/15-javascript-debugging-tips-and-tricks-that-will-make-you-a-better-developer-875a4146b119)