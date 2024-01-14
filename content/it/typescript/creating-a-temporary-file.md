---
title:    "TypeScript: Creazione di un file temporaneo"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Perché


Scrivere codice TypeScript può sembrare complicato per chi è alle prime armi, ma ci sono alcune funzionalità che possono semplificarci la vita durante lo sviluppo. Una di esse è la creazione di file temporanei. In questo articolo, esploreremo perché dovresti considerare di creare un file temporaneo e come farlo utilizzando TypeScript.

## Come fare

Per creare un file temporaneo in TypeScript, possiamo utilizzare la libreria `fs` del Node.js. Questa libreria ci permette di accedere alle funzionalità del filesystem e, tra queste, c'è anche la possibilità di creare file temporanei. Ecco un esempio di codice che crea un file temporaneo e ci mostra il suo nome una volta creato:

```TypeScript
const fs = require('fs');

fs.mkdtemp('/tmp/ts-temp', (err, folder) => {
  if (err) throw err;

  console.log(`File temporaneo creato in ${folder}`);
});
```

Dopo aver eseguito questo codice, nella console dovrebbe apparire un messaggio simile a questo:

`File temporaneo creato in /tmp/ts-tempxyz`

Come puoi vedere, la funzione `mkdtemp` ci restituisce un percorso alla cartella in cui il file temporaneo è stato creato.

## Approfondimento

Oltre a creare un file temporaneo utilizzando la libreria `fs`, possiamo anche specificare il prefisso e il suffisso del nome del file. In questo modo, possiamo identificare facilmente il file temporaneo in mezzo ad altri file. Ecco un esempio di codice che ci mostra come fare:

```TypeScript
const fs = require('fs');

const temporaryFile = require('tempfile');

temporaryFile('.txt', (err, path) => {
  if (err) throw err;

  console.log(`File temporaneo creato in ${path}`);
});
```

In questo caso, il file temporaneo verrà creato con un nome come `tmp-12345.txt`, dove "12345" è un numero a caso generato dalla libreria. In questo modo, possiamo facilmente identificare il file temporaneo e assicurarci di eliminarlo una volta che non ci serve più.

## Vedi anche

- [Documentazione ufficiale di Node.js su `fs` (link in inglese)](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html#fs_fs_mkdtemp_prefix_options_callback)
- [Documentazione ufficiale di Node.js su `tempfile` (link in inglese)](https://github.com/sindresorhus/tempfile#readme)
- [Che cos'è un file temporaneo? (link in italiano)](https://blog.iothings.com/che-cose-un-file-temporaneo/)