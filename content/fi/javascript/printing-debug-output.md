---
title:                "Vianjäljitystulostuksen tekeminen"
html_title:           "Javascript: Vianjäljitystulostuksen tekeminen"
simple_title:         "Vianjäljitystulostuksen tekeminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Debug-tulostuksen tulostaminen on tärkeä osa ohjelmointia, koska se auttaa meitä tunnistamaan virheitä ja korjaamaan niitä. Se on myös kätevä tapa seurata koodin suoritusta ja varmistaa sen oikeellisuus.

## Kuinka

Debug-tulostuksen tulostaminen on helppoa JavaScriptillä. Käyttämällä `console.log()` -funktiota voimme tulostaa viestejä ja muuttujien arvoja selaimen kehittäjätyökaluihin. Esimerkiksi:

```Javascript
let name = "Maija";
console.log("Tervetuloa, " + name);
```

Tämä tulostaa consoliin "Tervetuloa, Maija". Voimme myös tulostaa monimutkaisempia objekteja, kuten taulukoita tai json-tiedostoja:

```Javascript
let numbers = [1, 2, 3, 4, 5];
console.log("Taulukon sisältö: ", numbers);
```

Tämä tulostaa consoliin "Taulukon sisältö: [1, 2, 3, 4, 5]". Tällä tavalla voimme seurata vaihtuvien arvojen muutoksia ja varmistaa, että koodi toimii odotetusti.

## Syvemmälle

On tärkeää huomata, että `console.log()` -funktio ei toimi kaikilla selaimilla. Tällöin voimme käyttää `debugger` -komentoa, joka pysäyttää koodin suorituksen ja antaa meille mahdollisuuden tarkastella muuttujien arvoja ja suoritusjärjestystä. Esimerkiksi:

```Javascript
let number = 5;
debugger;
number += 10;
console.log(number);
```

Tässä tapauksessa, kun debugger saavutetaan, voimme tarkastella number-muuttujan arvoa ja sen muutosta.

Voimme myös käyttää `console.assert()` -funktiota, joka tarkistaa väittämän oikeellisuuden ja tulostaa virheviestin, jos se ei ole totta. Esimerkiksi:

```Javascript
console.assert(5 > 10, "5 ei voi olla suurempi kuin 10");
```

Tämä tulostaa consoliin "Assertion failed: 5 ei voi olla suurempi kuin 10". Tämä on hyödyllinen tapa varmistaa, että koodissamme olevat oletukset ovat oikein.

## Katso myös

- [W3Schools: JavaScript Debugging](https://www.w3schools.com/js/js_debugging.asp)
- [MDN Web Docs: console.log()](https://developer.mozilla.org/en-US/docs/Web/API/Console/log)
- [MDN Web Docs: Debugger](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/debugger)
- [MDN Web Docs: console.assert()](https://developer.mozilla.org/en-US/docs/Web/API/Console/assert)