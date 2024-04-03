---
date: 2024-01-20 17:55:35.992376-07:00
description: "How to: \u0141apcie kr\xF3tki przyk\u0142ad u\u017Cycia `fs` i `readFile`\
  \ z `promises` (do unikni\u0119cia callback hell)."
lastmod: '2024-03-13T22:44:35.157201-06:00'
model: gpt-4-1106-preview
summary: "\u0141apcie kr\xF3tki przyk\u0142ad u\u017Cycia `fs` i `readFile` z `promises`\
  \ (do unikni\u0119cia callback hell)."
title: Odczytywanie pliku tekstowego
weight: 22
---

## How to:
Łapcie krótki przykład użycia `fs` i `readFile` z `promises` (do uniknięcia callback hell).

```TypeScript
import { promises as fsPromises } from 'fs';

async function readTextFile(filePath: string): Promise<string> {
  try {
    const data = await fsPromises.readFile(filePath, 'utf8');
    return data;
  } catch (error) {
    throw new Error('Error reading the file');
  }
}

// Użycie
const filePath = './sample.txt';

readTextFile(filePath)
  .then((content) => console.log(content))
  .catch((error) => console.error(error));
```

Gdybyśmy mieli plik `sample.txt` z tekstem "Witaj, świecie!", output będzie:
```
Witaj, świecie!
```

## Deep Dive:
Czytanie plików tekstowych w TypeScript ma długą historię, głównie związana z Node.js, który dostarczał API dla operacji na plikach. Wcześniej rozpowszechnione były callbacki, które mogły prowadzić do zagnieżdżonych konstrukcji, nazywanych "callback hell". Dziś preferowane są `Promise` i `async/await` dla lepszej czytelności i obsługi błędów.

Alternatywy dla `fs` obejmują biblioteki zewnętrzne, takie jak `fs-extra` czy strumienie (streams), które lepiej nadają się do pracy z dużymi plikami. Strumienie pozwalają na odczyt danych w cząstkach, nie obciążając pamięci całym plikiem na raz.

W implementacji warto zwrócić uwagę na kodowanie pliku (standardowo `utf8`), ponieważ ignorowanie tego może prowadzić do problemów z niepoprawnie wyświetlającymi się znakami.

## See Also:
- Node.js `fs` module documentation: [Node.js File System](https://nodejs.org/api/fs.html)
- `fs-extra` library: [fs-extra](https://github.com/jprichardson/node-fs-extra)
- Understanding `async/await` in TypeScript: [Async/Await - Mozilla](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
