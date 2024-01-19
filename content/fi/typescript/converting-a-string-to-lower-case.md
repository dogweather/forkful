---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Muunnetaan merkkijono pieniksi kirjaimiksi, se tarkoittaa, että kaikki merkkijonossa olevat iso kirjaimet muutetaan pieniksi kirjaimiksi. Ohjelmoijat tekevät tämän yleensä, kun he haluavat vertailla tai käsitellä merkkijonoja tapa-herkästi.

## Näin se tehdään:
TypeScriptissä voit käyttää `.toLowerCase()` menetelmää muuttaaksesi merkkijonon pieniksi kirjaimiksi.
```TypeScript
let sana: string = "MOI MAAILMA";
let pienetKirjaimet: string = sana.toLowerCase();
console.log(pienetKirjaimet);
```
Koodinpätkän tulostus on 
```
"moi maailma"
```

## Syvää Sukeltamista
Merkkijonon muuntaminen pieniksi kirjaimiksi on yleinen toiminto, jota on käytetty ohjelmoinnissa jo pitkään. Sitä käytetään usein tietojen normalisointiin ennen niiden vertailua tai tallennusta. TypeScriptissä `.toLowerCase()` menetelmä periytyy JavaScriptin `String` luokasta.

Vaihtoehtoisesti, voit käyttää `.toLocaleLowerCase()` menetelmää. Se tekee saman asian kuin `.toLowerCase()`, mutta ottaa huomioon käyttäjän paikallisen ympäristön (locale).

Itse `.toLowerCase()` menetelmän toteutus riippuu selaimesta tai JavaScript-ympäristöstä. Yleensä se käy läpi merkkijonon merkit yksi kerrallaan ja muuttaa jokaisen ison kirjaimen vastaavaksi pieneksi kirjaimeksi.

## Katso Myös
1. JavaScriptin `String` luokan dokumentaatio: [MDN Website](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
2. TypeScriptin virallinen dokumentaio: [TypeScript Official Website](https://www.typescriptlang.org/docs/)