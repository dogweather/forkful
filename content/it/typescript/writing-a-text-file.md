---
title:                "Scrivere un file di testo"
date:                  2024-01-19
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere un file di testo significa salvare dati in un formato leggibile. Programmatori lo fanno per persistenza dati, configurazioni, logging o scambio di informazioni.

## How to:
TypeScript usa Node.js per scrivere su file. Installa prima `fs` con `npm install @types/node`.

```TypeScript
import { writeFile } from 'fs';

const data = 'Ciao mondo!';

writeFile('testo.txt', data, (err) => {
  if (err) throw err;
  console.log('File salvato!');
});
```
Output:
```
File salvato!
```

Per scrivere in modo sincrono:
```TypeScript
import { writeFileSync } from 'fs';

const data = 'Ciao di nuovo!';

try {
  writeFileSync('testo.txt', data);
  console.log('File salvato!');
} catch (err) {
  console.error('Errore:', err);
}
```
Output:
```
File salvato!
```

## Deep Dive
Scrivere su file è essenziale da quando i computer esistono. In TypeScript, `fs.writeFile` è asincrono, utile per evitare blocchi. `fs.writeFileSync` va usato quando è necessaria l'esecuzione sincrona. Attenzione con grandi dati - considera `streams`.

## See Also
- Documentazione Node.js 'fs': [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- Streams in Node.js: [https://nodejs.org/api/stream.html](https://nodejs.org/api/stream.html)
