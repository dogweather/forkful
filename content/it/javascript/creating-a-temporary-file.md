---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché?

Creare un file temporaneo significa generare un file che contiene dati di supporto per lo svolgimento di un task. I programmatori lo fanno per gestire le risorse di sistema in maniera più efficiente.

## Ecco Come:

Si può creare un file temporaneo in Javascript con il modulo `fs` di Node.js. Ecco un breve esempio:

```Javascript
const fs = require('fs');

fs.mkdtemp('/tmp/foo-', (err, folder) => {
  if (err) throw err;
  console.log(folder);
});
```

Nel codice sopra, `'/tmp/foo-'` è il prefisso del percorso del file temporaneo. L'output sarà simile a `/tmp/foo-XYZ`.

## Approfondimento:

Creare file temporanei è un'abitudine di lunga data nel mondo della programmazione. Un'alternativa a `fs.mkdtemp` è usare librerie come `tmp` o `temp`, che offrono un'interfaccia più semplice e convenienti opzioni di pulizia automatica. Si noti che mentre `fs.mkdtemp` crea una cartella temporanea, per generare file temporanei si può usare `fs.open` con l'opzione `'wx'`.

```Javascript
const fs = require('fs');

fs.open('/tmp/foo-', 'wx', (err, fd) => {
  if (err) throw err;
  console.log(fd);
});
```

## Vedi Anche:

- Documentazione Node.js su fs.mkdtemp: https://nodejs.org/api/fs.html#fs_fs_mkdtemp_prefix_options_callback
- Libreria `tmp`: https://www.npmjs.com/package/tmp
- Libreria `temp`: https://www.npmjs.com/package/temp