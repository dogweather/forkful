---
title:                "Tekstitiedoston lukeminen"
date:                  2024-01-20T17:55:22.953079-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstitiedoston lukeminen tarkoittaa tekstin kutsumista tiedostosta ohjelman käytettäväksi. Ohjelmoijat tekevät tämän, jotta he voivat käsitellä tai analysoida tiedon sisältöä automaattisesti.

## How to:
Asennetaan ensin node:n tyypitykset:

```bash
npm install --save @types/node
```

Sitten käytetään Node.js:n `fs`-moduulia:

```TypeScript
import * as fs from 'fs';

function readTextFile(filePath: string): void {
  fs.readFile(filePath, 'utf-8', (err, data) => {
    if (err) {
      console.error('Tiedoston lukeminen epäonnistui:', err);
      return;
    }
    console.log(data);
  });
}

// Käytä funktiota
readTextFile('example.txt');
```

Esimerkkitiedostosta `example.txt` tulostuu:

```
Hei, tässä on esimerkkitekstiä.
```
## Deep Dive
Tekstitiedoston luku on perusosa ohjelmointia; se on tehty 1950-luvulta lähtien. Vaihtoehtoja `fs.readFile`-funktiolle on olemassa, kuten synkroninen `fs.readFileSync` tai moderneja lähestymistapoja kuten `fs.promises.readFile`, joka tukee `async/await` -syntaksia:

```TypeScript
import { promises as fsPromises } from 'fs';

async function readTextFileAsync(filePath: string): Promise<void> {
  try {
    const data = await fsPromises.readFile(filePath, 'utf-8');
    console.log(data);
  } catch (err) {
    console.error('Tiedoston lukeminen epäonnistui:', err);
  }
}

// Käytä funktiota
readTextFileAsync('example.txt');
```

Tärkeää on valita menetelmä, joka parhaiten sopii sovelluksesi vaatimuksiin. Blocking (esim. `readFileSync`) vs non-blocking (esim. `readFile`), suorituskyky ja koodin selkeys ovat kaikki harkittavia asioita.

## See Also
Node.js `fs`-dokumentaatio:

- [Node.js File System docs](https://nodejs.org/api/fs.html)
- [Asynkronisen ohjelmoinnin oppaat](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous)

TypeScriptin alkeet:

- [TypeScriptin viralliset dokumentit](https://www.typescriptlang.org/docs/)
