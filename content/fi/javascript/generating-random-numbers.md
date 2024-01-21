---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:49:23.009318-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Satunnaislukujen generointi on koodinpätkä, joka tuottaa ennustamattomia numeroita. Kehittäjät tarvitsevat näitä esimerkiksi peleissä, datan anonymisoinnissa tai turvallisuudessa.

## How to: - Näin teet sen:
```Javascript
// Perus Math.random()
let randomNum = Math.random();
console.log(randomNum); // Output: 0.5052417714466442, tai muu satunnaisluku väliltä 0-1

// Satunnainen kokonaisluku väliltä min ja max
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 10)); // Output: esim. 7
```

## Deep Dive - Syväsukellus:
Historiallisesti satunnaislukujen luonti ei ole ollut 'oikeasti' satunnaista, vaan pseudosatunnaista – tietokoneet eivät ole niin arvaamattomia kuin luullaan.

Alternatiiveiksi `Math.random()`-funktiolle on tarjolla erilaisia kirjastoja, jotka voivat tuottaa laadukkaampia satunnaislukusekvenssejä. Esimerkiksi `crypto`-moduuli Node.js:ssä:

```Javascript
const crypto = require('crypto');

function randomIntFromCrypto(min, max) {
  const range = max - min;
  const bytesNeeded = Math.ceil(Math.log2(range) / 8);
  const randomBytes = parseInt(crypto.randomBytes(bytesNeeded).toString('hex'), 16);
  return Math.floor(randomBytes / Math.pow(2, bytesNeeded * 8) * (range + 1)) + min;
}

console.log(randomIntFromCrypto(1, 10)); // Output: esim. 5
```

Tätä pidetään turvallisempana, sillä se käyttää salausstandardien mukaisia satunnaislukualgoritmeja.

## See Also - Katso Myös:
- MDN Web Docs tuotteesta Math.random: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- Node.js: `crypto`-moduuli: https://nodejs.org/api/crypto.html
- Stack Overflow - paljon keskustelua erilaisista generaattoreista ja niiden käyttötarkoituksista: https://stackoverflow.com