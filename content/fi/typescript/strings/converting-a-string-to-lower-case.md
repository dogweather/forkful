---
date: 2024-01-20 17:39:12.372251-07:00
description: "How to - Kuinka tehd\xE4\xE4n ."
lastmod: '2024-04-05T21:53:57.860331-06:00'
model: gpt-4-1106-preview
summary: ''
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
weight: 4
---

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
