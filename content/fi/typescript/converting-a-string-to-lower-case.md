---
title:                "Muuttaminen merkkijonoksi pienellä kirjoitettuna"
html_title:           "TypeScript: Muuttaminen merkkijonoksi pienellä kirjoitettuna"
simple_title:         "Muuttaminen merkkijonoksi pienellä kirjoitettuna"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonon muuntaminen pieniksi kirjaimiksi tarkoittaa, että kaikki merkkijonossa olevat kirjaimet muutetaan pieniksi kirjaimiksi. Tätä tehdään usein ohjelmoinnissa, jotta voidaan helpommin vertailla merkkijonoja tai tehdä niiden kanssa erilaisia toimintoja.

## Kuinka:

```TypeScript
const merkkijono = "Tämä On Esimerkki";
const pienetKirjaimet = merkkijono.toLowerCase();

console.log(pienetKirjaimet); // Output: "tämä on esimerkki"
```

## Syvempi sukellus:

Merkkijonon muuntaminen pieniksi kirjaimiksi on ollut osa ohjelmointia jo pitkään ja se on yksi yleisimmistä merkkijonojen käsittelytoiminnoista. Aiemmin tähän tarkoitukseen on käytetty esimerkiksi C-kielen toUpper-funktiota. TypeScriptissä merkkijonon muuntaminen pieniksi kirjaimiksi tuo etuna mukavamman ja helpommin luettavan koodin.

## Katso myös:

- [String.prototype.toLowerCase() - MDN Web Docs](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [TypeScriptin virallinen dokumentaatio](https://www.typescriptlang.org/docs/home.html)