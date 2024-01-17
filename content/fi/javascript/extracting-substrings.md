---
title:                "Alimerkkijonojen erottaminen"
html_title:           "Javascript: Alimerkkijonojen erottaminen"
simple_title:         "Alimerkkijonojen erottaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Substringien eristäminen on prosessi, jossa valitaan ja paljastetaan tietty osa merkkijonosta. Ohjelmoijat tekevät näin esimerkiksi lopullisen tulosteen muotoilun tai tietojen käsittelyn vuoksi. Se on yleinen tekniikka, jota käytetään laajasti monissa ohjelmointikielessä.

## Kuinka tehdä?

```Javascript
let merkkijono = "Tämä on esimerkkiteksti";
let substr = merkkijono.substring(5, 14);

console.log(substr); // tulostaa "on esimerk"
```

- Määritä ensin muuttuja, joka sisältää merkkijonon, josta haluat eristää substringin.
- Käytä `substring()` -funktiota ja anna sille aloitus- ja lopetusindeksit alueelle, jonka haluat eristää.
- Tallenna tulos muuttujaan ja tulosta se käyttämällä `console.log()` -funktiota.

## Syvällinen sukellus

- Substringien eristämisellä on pitkä historia, ja se on tullut yhä tärkeämmäksi nykyaikaisessa ohjelmoinnissa.
- Vaikka `substring()` on yleinen ja kätevä funktio, on myös muita tapoja eristää substringeja, kuten `slice()` ja `substr()`.
- Käytä `substring()` -funktiota varovaisesti ja muista ottaa huomioon indeksoinnin välittäminen.

## Katso myös

Jos haluat lisätietoja substringien eristämisestä Javascriptillä, tutustu seuraaviin lähteisiin:

- [MDN Web Docs: substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [W3Schools: JavaScript substring()](https://www.w3schools.com/jsref/jsref_substring.asp)
- [JavaScript.info: Substrings and substr](https://javascript.info/string-substr)