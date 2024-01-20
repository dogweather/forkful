---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

# Debug-tulosteen tulostaminen JavaScriptissa

## Mitä & Miksi?
Debug-tulosteen tulostaminen on koodin lähettämää viestiä ohjelmoinnin aikana. Ohjelmoijat käyttävät sitä tunnistaakseen ja korjatakseen virheitä koodissa.

## Kuinka Toimii:
Käytä `console.log()`-funktiota tulostaaksesi viestejä konsoliin:

```Javascript
console.log("Hello, World!");
```

Tämä tulostaa:

```
Hello, World!
```

## Deep Dive:
Debug-tulosteen tulostaminen on olennainen osa ohjelman kehitystä. Suosittu JavaScriptin `console.log()` -funktio on peräisin Netscape Navigator -selaimen JS-moottorista. Sen avulla voit tulostaa monentyyppisiä tietoja, välittämättä siitä, onko se kokonainen objekti vai yksittäinen muuttuja.

Vaihtoehtoina `console.log()`:lle ovat `console.debug()`, `console.info()`, `console.warn()` ja `console.error()`, joiden avulla voidaan erottaa debug-viestin vakavuustasot.

Kuitenkin, pidä mielessä, että liiallinen konsoliin tulostaminen voi hidastaa suorituskykyä, erityisesti silloin kun tulostetaan suuria tietomääriä tai kun tulostetaan monimutkaisia objekteja, joissa on paljon syväkopiointia.

## Katso Myös:
Seuraavat linkit tarjoavat lisätietoja ja kontekstia:

1. MDN: Console - https://developer.mozilla.org/fi/docs/Web/API/Console
2. JavaScript Info: Debugging - https://javascript.info/debugging-chrome
3. TechRepublic: Remove console.log to improve JS performance - https://www.techrepublic.com/article/how-to-improve-javascript-performance-by-removing-console-log-statements/