---
date: 2024-01-20 17:48:35.732035-07:00
description: "Merkkijonon pituuden selvitt\xE4minen tarkoittaa sen merkkim\xE4\xE4\
  r\xE4n laskemista. Ohjelmoijat tarvitsevat t\xE4t\xE4 tietoa validointiin, rajauksiin\
  \ ja tiedon\u2026"
lastmod: '2024-03-13T22:44:56.306657-06:00'
model: gpt-4-1106-preview
summary: "Merkkijonon pituuden selvitt\xE4minen tarkoittaa sen merkkim\xE4\xE4r\xE4\
  n laskemista."
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

## How to: (Kuinka tehdä:)
```TypeScript
let greeting: string = "Hei maailma!";
let lengthOfGreeting: number = greeting.length;

console.log(lengthOfGreeting); // 12
```

## Deep Dive (Syväsukellus)
TypeScript perustuu JavaScriptiin, missä joka merkkijonolla on `.length`-ominaisuus, josta saa suoraan sen pituuden. Historiallisesti tämä on ollut nopein tapa selvittää merkkijonon pituus. Vaihtoehtoisia menetelmiä, kuten silmukoiden läpikäyminen, ovat hitaampia ja tarpeettomia, kun `.length` on saatavilla. Unicode-merkkien käsittelyn myötä `.length` ei aina anna "oikeaa" merkkimäärää erityisesti monimutkaisten tai yhdistettyjen Unicode-merkkien kanssa. Tällöin kehittäjät voivat käyttää `Array.from`-funktiota tai String Iterator -protokollaa tarkemman pituuden määrittämiseen.

```TypeScript
let complexString: string = "👨‍👩‍👦";
let realLength: number = Array.from(complexString).length;

console.log(realLength); // 1, odotettu pituus perhe-emojin
```

## See Also (Katso Myös)
- Mozilla Developer Network (MDN) Web Docs: [String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- TypeScript-ohjeet: [Basic Types](https://www.typescriptlang.org/docs/handbook/basic-types.html)
- Unicode-standardi: [Unicode strings](https://unicode.org/reports/tr18/#Unicode_Sets)
