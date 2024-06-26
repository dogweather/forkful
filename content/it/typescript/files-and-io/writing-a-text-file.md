---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:32.763992-07:00
description: "Come fare: TypeScript di per s\xE9 non gestisce direttamente le operazioni\
  \ sui file poich\xE9 viene compilato in JavaScript, che tradizionalmente viene eseguito\u2026"
lastmod: '2024-03-13T22:44:43.194347-06:00'
model: gpt-4-0125-preview
summary: "TypeScript di per s\xE9 non gestisce direttamente le operazioni sui file\
  \ poich\xE9 viene compilato in JavaScript, che tradizionalmente viene eseguito nel\
  \ browser con accesso limitato al sistema di file."
title: Scrivere un file di testo
weight: 24
---

## Come fare:
TypeScript di per sé non gestisce direttamente le operazioni sui file poiché viene compilato in JavaScript, che tradizionalmente viene eseguito nel browser con accesso limitato al sistema di file. Tuttavia, quando utilizzato in un ambiente Node.js, il modulo `fs` (File System) offre funzionalità per scrivere file.

### Utilizzando il modulo fs di Node.js
Prima di tutto, assicurati di lavorare in un ambiente Node.js. Poi, utilizza il modulo `fs` per scrivere file di testo. Ecco un esempio di base:

```typescript
import * as fs from 'fs';

const data = 'Ciao, mondo!';
const filePath = './message.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('Il file è stato salvato!');
});
```

Questo scriverà in modo asincrono "Ciao, mondo!" in `message.txt`. Se il file non esiste, Node.js lo crea; se esiste, Node.js lo sovrascrive.

Per la scrittura sincrona di file, usa `writeFileSync`:

```typescript
import * as fs from 'fs';

const data = 'Ciao di nuovo, mondo!';
const filePath = './message.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('Il file è stato salvato!');
} catch (err) {
    console.error(err);
}
```

### Utilizzando popolari librerie di terze parti
Sebbene il modulo nativo `fs` sia potente, alcuni sviluppatori preferiscono usare librerie di terze parti per ulteriore comodità e funzionalità. `fs-extra` è una scelta popolare che estende `fs` e rende le operazioni sui file più semplici.

Prima di tutto, dovrai installare `fs-extra`:

```
npm install fs-extra
```

Poi, puoi usarlo nel tuo file TypeScript per scrivere contenuto di testo:

```typescript
import * as fs from 'fs-extra';

const data = 'Questo è fs-extra!';
const filePath = './extraMessage.txt';

// Usando async/await
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('Il file è stato salvato con fs-extra!');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

Questo snippet di codice fa la stessa cosa degli esempi `fs` precedenti ma utilizza la libreria `fs-extra`, offrendo una sintassi più pulita per gestire le promesse.
