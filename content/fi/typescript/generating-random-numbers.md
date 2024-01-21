---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:50:14.838952-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Satunnaislukujen generointi tarkoittaa uusien, ennustamattomien numeroiden luomista. Ohjelmoijat käyttävät niitä simuloimaan arpajaisia, kehittämään pelejä ja toteuttamaan kryptografiaa.

## How to - Kuinka?
TypeScriptissa satunnaislukujen generointi tapahtuu sisäänrakennetun Math-objektin avulla. Tsekkaa esimerkit:

```TypeScript
// Yksinkertainen 0 ja 1 välillä
let randomBetween0and1 = Math.random();
console.log(randomBetween0and1);

// Satunnainen kokonaisluku väliltä 0-9
let randomIntFrom0To9 = Math.floor(Math.random() * 10);
console.log(randomIntFrom0To9);

// Satunnainen kokonaisluku kahden arvon väliltä (min ja max)
function getRandomInt(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}
console.log(getRandomInt(5, 15));
```

Tulosteet vaihtelevat joka ajokerralla:

```
0.437723uemvienfbv
3
11
```

## Deep Dive - Syväsukellus
JavaScriptissä `Math.random()` -funktio on se standardi satunnaislukujeneraattori, ja koska TypeScript on JavaScriptin superset, sama pätee myös siihen. Se palauttaa kelluvan pisteluvun välillä 0 (mukaan lukien) ja 1 (ei mukaan lukien).

Perinteisesti satunnaislukujen generointi on haastavaa tietokoneille, jotka ovat deterministisiä laitteita. Algoritmit kuten lineaarinen kongruenssimenetelmä ovat historian saatossa käytettyjä, mutta niiden tuottamat tulokset eivät ole loistavia. Vaikka `Math.random()` ei olekaan kryptografisesti turvallinen, se riittää useimpiin ei-kriittisiin sovelluksiin.

Jos taas tarvitset kryptografisesti vahvoja satunnaislukuja, voit käyttää Web Crypto API:n `crypto.getRandomValues` metodia, mikä toimii myös TypeScriptissä, mutta tarvitsee selainympäristön.

```TypeScript
const array = new Uint32Array(1);
window.crypto.getRandomValues(array);
console.log(array[0]);
```

## See Also - Katso Myös
- [MDN Web Docs - Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [MDN Web Docs - Crypto.getRandomValues()](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)