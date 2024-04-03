---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:35.569646-07:00
description: "Scrivere un file di testo in JavaScript riguarda spesso la creazione\
  \ e il salvataggio di dati in un formato semplice e leggibile per registrare, esportare\u2026"
lastmod: '2024-03-13T22:44:43.831966-06:00'
model: gpt-4-0125-preview
summary: Scrivere un file di testo in JavaScript riguarda spesso la creazione e il
  salvataggio di dati in un formato semplice e leggibile per registrare, esportare
  input dell'utente o per scopi di configurazione.
title: Scrivere un file di testo
weight: 24
---

## Come fare:
In un ambiente Node.js, puoi usare il modulo integrato `fs` (File System) per scrivere file di testo. Questo esempio dimostra come scrivere testo in un file in modo asincrono:

```javascript
const fs = require('fs');

const data = 'Ciao, Mondo! Questo è il testo da scrivere in un file.';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('Il file è stato scritto.');
});
```

Output di esempio:
```
Il file è stato scritto.
```

Per la scrittura sincrona di file, usa `writeFileSync`:
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('Il file è stato scritto.');
} catch (error) {
  console.error('Errore nella scrittura del file:', error);
}
```

Nei moderni browser web, l'API Accesso al File System introduce la possibilità di leggere e scrivere file. Tuttavia, il suo utilizzo è soggetto ai permessi dell'utente. Ecco come creare e scrivere su un file:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Ciao, Mondo! Questo è la scrittura di file di testo nel browser.');
  await writable.close();
}
```

Per scenari più complessi o quando si lavora con file di grandi dimensioni, potresti optare per librerie di terze parti come `FileSaver.js` per i browser:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Ciao, Mondo! Questo è testo da FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

Ricorda, scrivere file sul lato client (nei browser) è limitato a causa di preoccupazioni per la sicurezza, e qualsiasi operazione che richiede di salvare sul disco locale dell'utente richiederà solitamente il loro esplicito permesso.
