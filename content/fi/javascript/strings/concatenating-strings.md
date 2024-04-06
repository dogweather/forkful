---
date: 2024-01-20 17:34:57.429032-07:00
description: "How to - Kuinka Tehd\xE4: ."
lastmod: '2024-04-05T21:53:58.520313-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Merkkijonojen yhdist\xE4minen"
weight: 3
---

## How to - Kuinka Tehdä:
```javascript
let tervehdys = "Hei";
let nimi = "Maija";
let lause = tervehdys + ", " + nimi + "!"; // Vanha tapa
console.log(lause); // "Hei, Maija!"

// ES6 Template literals
let lause_uusi = `${tervehdys}, ${nimi}!`; // Uudempi tapa
console.log(lause_uusi); // "Hei, Maija!"
```

## Deep Dive - Syväsukellus:
Stringien yhdistämisen juuret ovat ohjelmointikielten alkuajoissa. Ennen moderneja ohjelmointikieliä, kuten JavaScript, oli useampia rajoittuneita keinoja muodostaa dynaamisia tekstejä. JavaScriptissä, yksinkertainen operaattori `+` oli alun perin suunniteltu stringien yhdistämiseen. Myöhemmin, ECMAScript 6 (ES6) toi mukanaan Template literalsin (template-litteraalit), joka tekee stringien yhdistämisestä vähemmän sekavaa ja monipuolisempaa.

- Historia: Yksinkertainen `+` operaattori on ollut käytössä vuosikymmeniä.
- Vaihtoehdot: Ennen ES6:ta, `concat()` metodia käytettiin, mutta se oli vähemmän suosittu sen kömpelön syntaksin vuoksi.
- Toteutuksen yksityiskohdat: Template literals mahdollistavat monirivisen tekstinsyötön ja ilmaisujen upottamisen suoraan merkkijonoihin, mikä tekee koodista selkeämpää.

## See Also - Katso Myös:
- MDN Web Docs String concatenation: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- Template literals (Template strings) MDN: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
