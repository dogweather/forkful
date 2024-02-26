---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:17.934729-07:00
description: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) JavaScriptiss\xE4 ovat malleja,\
  \ joita k\xE4ytet\xE4\xE4n merkkijonoissa merkkiyhdistelmien etsimiseen. Ohjelmoijat\
  \ k\xE4ytt\xE4v\xE4t niit\xE4\u2026"
lastmod: '2024-02-25T18:49:53.846956-07:00'
model: gpt-4-0125-preview
summary: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) JavaScriptiss\xE4 ovat malleja,\
  \ joita k\xE4ytet\xE4\xE4n merkkijonoissa merkkiyhdistelmien etsimiseen. Ohjelmoijat\
  \ k\xE4ytt\xE4v\xE4t niit\xE4\u2026"
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Säännölliset lausekkeet (regex) JavaScriptissä ovat malleja, joita käytetään merkkijonoissa merkkiyhdistelmien etsimiseen. Ohjelmoijat käyttävät niitä tekstihakujen tekemiseen, tekstin poimintaan ja manipulointiin, mahdollistaen tehokkaat merkkijonon käsittelyoperaatiot tiiviillä koodilla.

## Kuinka:

### Perusvastaavuus

Aloittaaksesi voit luoda yksinkertaisen regex-mallin ja käyttää sitä löytääksesi vastaavuuksia merkkijonosta. Tässä etsimme sanaa "code":

```javascript
const str = "I love to code in JavaScript.";
const pattern = /code/;
const result = pattern.test(str);
console.log(result); // true
```

### Käyttäen `String.prototype.match()`

Saadaksesi taulukon vastaavuuksista:

```javascript
const matches = str.match(/code/);
console.log(matches[0]); // "code"
console.log(matches.index); // 10
```

### Globaali haku

Löytääksesi kaikki vastaavuudet, käytä `g`-lippua:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### Suur- ja pienaakkosista piittaamaton haku

`i`-lippu ohittaa kirjainkoon:

```javascript
const caseInsensitiveMatch = "JavaScript is fun".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### Tekstin korvaaminen

Käytä `String.prototype.replace()` korvataksesi osia merkkijonosta:

```javascript
const newStr = "JavaScript is fun".replace(/fun/, "awesome");
console.log(newStr); // "JavaScript is awesome"
```

### Ryhmien käyttö

Ryhmillä voidaan tallentaa osia mallista:

```javascript
const groupedPattern = /(\w+) is (\w+)/;
const replaceWithGroups = "JavaScript is fun".replace(groupedPattern, "$2 is $1");
console.log(replaceWithGroups); // "fun is JavaScript"
```

### Ulkopuoliset kirjastot

Vaikka JavaScriptin sisäänrakennetut regex-ominaisuudet ovat tehokkaita, jotkut tehtävät saattavat yksinkertaistua kirjastoilla, kuten `XRegExp`. Se tarjoaa lisäsyntaksia ja lippuja, tekee monimutkaisista malleista luettavampia:

```javascript
// XRegExp-kirjaston esimerkki
const XRegExp = require('xregexp');
const str = "Cats are fantastic.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Cats", "are", "fantastic"]
```

Tämä katkelma esittelee `XRegExp`in käyttöä kaikkien Unicode-sanojen vastaavuuksien etsimiseen merkkijonosta, esitellen kirjaston kyvyn käsitellä laajennettuja merkistökokoelmia JavaScriptin sisäänrakennettujen ominaisuuksien ulkopuolella.
