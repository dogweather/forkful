---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "TypeScript: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Säännöllisiä lausekkeita käytetään tekstin haun, muokkauksen ja korvaamisen helpottamiseen. Ne ovat voimakas työkalu ohjelmoijille, jotka haluavat tehdä tiettyjen merkkijonojen käsittelystä nopeampaa ja tehokkaampaa.

## Miten:
## Miten:
ES7 standardin myötä TypeScript-tuki säännöllisille lausekkeille tuli mahdolliseksi. Voit luoda uusia säännöllisiä lausekkeita käyttämällä RegExp-konstruktoria, ja sitten käyttää erilaisia metodeja, kuten `test()` ja `exec()`, tekstin käsittelyyn. Esimerkiksi:

```TypeScript
let regex: RegExp = /t[ea]st/;
let str: string = "test";
regex.test(str); // Output: true
```

## Syvempi sukellus:
Ennen ES7-standardia, TypeScript-tuki säännöllisille lausekkeille oli mahdollista vain toteuttamalla lisäosana. Säännölliset lausekkeet ovat myös yleinen tapa käsitellä merkkijonoja muissa ohjelmointikielissä, kuten JavaScriptissa ja Perlissä.

Vaihtoehtoja säännöllisille lausekkeille ovat esimerkiksi tietokannat tai muut erityisesti merkkijonojen käsittelyyn tarkoitetut kirjastot. TypeScriptissä tärkeä asia on myös ymmärtää, että säännölliset lausekkeet ovat vain yksi työkalu monien joukossa ja niitä ei välttämättä tarvitse käyttää jokaisessa ohjelmassa.

Säännölliset lausekkeet toimivat käytännössä haku- tai korvausmallina, jonka avulla voit muokata merkkijonoja nopeammin ja tehokkaammin kuin manuaalisesti etsimällä ja korvaamalla tiettyjä merkkijonoja.

## Katso myös:
- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Documentation: Regular Expressions](https://www.typescriptlang.org/docs/handbook/declarations.html#regular-expressions)
- [Stack Overflow: How do Regular Expressions work in TypeScript](https://stackoverflow.com/questions/60829990/how-do-regular-expressions-work-in-typescript)