---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
aliases:
- fi/typescript/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:12.372251-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä ja Miksi?
Muuttaa merkkijono pieniksi kirjaimiksi. Tehdään johdonmukaisuuden ja vertailun helpottamiseksi.

## How to - Kuinka tehdään
```TypeScript
let greeting: string = "Hei Maailma!";
let lowerCaseGreeting: string = greeting.toLowerCase();
console.log(lowerCaseGreeting);  // "hei maailma!"
```

## Deep Dive - Sukellus syvemmälle
JavaScript, ja siten TypeScript, on käyttänyt `.toLowerCase()` metodia pienentääkseen kirjaimet jo vuosien ajan. Tämä on osa standardia ECMAScript-kirjastoa, mikä tarkoittaa että se on pysynyt suhteellisen muuttumattomana.

Vaihtoehtoina voidaan käyttää esimerkiksi `.toLocaleLowerCase()`, joka huomioi käyttäjän lokalisoinnit eri kielialueilla. Tämän voi nähdä käytännössä, jos halutaan esimerkiksi muuntaa turkkilainen "İ" oikein pieneksi "i":ksi.

Toiminta tapahtuu luomalla uusi merkkijono, missä jokainen alkuperäisen merkkijonon kirjain on muunnettu vastaavaksi pieneksi kirjaimeksi käyttäen Unicode-standardia.

## See Also - Katso myös
- MDN Web Docs - String.prototype.toLowerCase(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- Unicode standard: https://www.unicode.org/standard/standard.html
- ECMAScript Language Specification: https://tc39.es/ecma262/
