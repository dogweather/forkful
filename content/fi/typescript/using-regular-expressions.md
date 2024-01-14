---
title:                "TypeScript: Säännöllisten lausekkeiden käyttö"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet TypeScriptin käyttäjä ja törmäät usein haasteisiin merkkijonojen käsittelyssä, säännölliset lausekkeet voivat olla pelastuksesi. Säännölliset lausekkeet ovat voimakas työkalu, joka helpottaa merkkijonojen manipulointia ja hakuja.

## Näin

Säännöllisiä lausekkeita voi käyttää TypeScriptissä käyttämällä `RegExp` -luokkaa ja sen ristiriitaisia ​​metodeja. Alla on esimerkkejä koodista ja niiden tuottamasta tulosteesta:

```TypeScript
// Luodaan uusi säännöllinen lauseke ja tallennetaan se muuttujaan
const säännöllinenLauseke = new RegExp('hello');
// Testataan, löytyykö lauseke annetusta merkkijonosta
console.log(säännöllinenLauseke.test('Hello World')); // tulostaa true
// Korvataan merkkijonossa esiintyvät hello-sanat goodbye-sanalla
console.log('Hello World'.replace(säännöllinenLauseke, 'goodbye')); // tulostaa goodbye World
```

Toinen hyödyllinen metodi on `exec()`, joka palauttaa havainnoidun lausekkeen löytyessä ja `null`, jos haku ei onnistunut:

```TypeScript
// Luodaan uusi säännöllinen lauseke ja tallennetaan se muuttujaan
const säännöllinenLauseke = new RegExp('last ([A-Za-z]+)');
// Etsitään merkkijonosta viimeistä sanaa ennen sanaa "last"
console.log(säännöllinenLauseke.exec('Hei, nimeni on John ja asun viimeisenä kaupungissa')); // tulostaa['last kaupungissa', 'kaupungissa']
```

## Syvemmälle

Säännölliset lausekkeet voivat olla monimutkaisia ​​ja kattavat suuren joukon erilaisia ​​merkkijonojen käsittelytapoja. Hyödynnä dokumentaatiota ja opettele seuraamaan säännöllisten lausekkeiden rakennetta ja logiikkaa.

## Katso myös

- [TypeScriptin säännölliset lausekkeet - virallinen dokumentaatio](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Learn the Basics of Regular Expressions in TypeScript](https://alligator.io/typescript/regular-expressions/)
- [Regex Tester - verkkosivusto säännöllisten lausekkeiden testaamiseen](https://regex101.com/)