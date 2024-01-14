---
title:                "TypeScript: Tiedoston lukeminen"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Finnish readers, have you ever wondered how to read a text file using TypeScript? Text files are commonly used for storing and sharing important information, but understanding how to read them using programming languages can be a valuable skill. In this blog post, we will explain the *why* behind reading a text file using TypeScript and provide step-by-step instructions on how to do it. 

## Miten

Aluksi luodaan tekstitiedosto "example.txt" ja kirjoitetaan siihen seuraavaa sisältöä:
```TypeScript
Tämä on tekstitiedoston esimerkki.
Tärkeää tietoa.
Loppu-
```

Nyt, kun meillä on tekstitiedosto, voimme aloittaa sen lukemisen TypeScriptillä:

1. Tuodaan tarvittava moduuli käyttäen "fs" kirjastoa:
```TypeScript
import fs from 'fs';
```
2. Luodaan muuttuja joka sisältää tekstitiedoston nimen:
```TypeScript
const tiedostoNimi = 'example.txt';
```
3. Käytetään "readFile" funktiota lukemaan tiedosto:
```TypeScript
fs.readFile(tiedostoNimi, 'utf8', (error, data) => {
  if (error) {
    throw error; // Jos tapahtuu virhe, heitetään se
  }
  console.log(data); // Tulostetaan tiedoston sisältö konsoliin
});
```
Tuloste:
```
Tämä on tekstitiedoston esimerkki.
Tärkeää tietoa.
Loppu-
```

## Syvempi sukellus

Kuten näimme edellä, tekstitiedoston lukeminen TypeScriptillä on melko yksinkertaista. Mutta mitä tehdä jos haluamme lukea tiedostoa rivi kerrallaan? Tässä tapauksessa voimme käyttää "createReadStream" funktiota ja "line-by-line" kirjastoa. Alla on esimerkki, joka tulostaa tiedoston jokaisen rivin konsoliin:
```TypeScript
import { createReadStream } from 'fs';
import { createInterface } from 'readline';
import { on } from 'events';
import { once } from 'events';
import { LineStream } from 'byline';

async function readLines() {
  const tiedostoNimi = 'example.txt';
  const input = createReadStream(tiedostoNimi);
  const output = new LineStream();

  const rl = createInterface({
    input,
    crlfDelay: Infinity,
    historySize: 0,
    output,
    terminal: false
  });

  rl.on('line', line => console.log(line)); // Tulostaa jokaisen rivin
  await once(rl, 'close'); // Sulkee lukemisen
}

readLines();
```
Tuloste:
```
Tämä on tekstitiedoston esimerkki.
Tärkeää tietoa.
Loppu-
```

## Katso myös

- [Node.js fs moduuli](https://nodejs.dev/learn/the-nodejs-fs-module)
- [Reading and Writing Files in TypeScript](https://blog.bitsrc.io/reading-and-writing-files-in-typescript-a50107692f58)