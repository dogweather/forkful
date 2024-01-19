---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cos'è & Perché? 

Creare un file temporaneo significa creare un file destinato ad essere cancellato dopo averlo utilizzato. I programmatori lo fanno per conservare temporaneamente i dati che non devono essere conservati permanentemente, come log, cache o file interstiziali. 

## Come fare:

Per creare un file temporaneo in TypeScript, possiamo utilizzare il modulo `tmp-promise` che semplifica la gestione dei file temporanei. Ecco un esempio:

```TypeScript
import { file } from 'tmp-promise';

async function createTempFile() {
    const { path, cleanup } = await file({ mode: 0o600, prefix: 'tmp-', postfix: '.txt' });
    
    console.log('Virtual file created at:', path);
    // remember to clean up
    await cleanup();
}
```

Ecco un esempio di output:

```
Virtual file created at: /tmp/tmp-1234abcd.txt
```

## Approfondimenti 

La creazione di file temporanei ha una lunga storia, risale ai primi giorni della programmazione e continua ad essere una pratica comune. Questo permette di risparmiare memoria e di evitare problemi di concorrenza, dato che i file temporanei sono unici ed eliminati dopo l'utilizzo. 

Esistono alternative a `tmp-promise`, come `mktemp` o `tempfile`. La scelta dipende dai tuoi specifici requisiti: `mktemp` fornisce più opzioni di configurazione, mentre `tempfile` è più semplice da usare. 

Riguardo all'implementazione, `tmp-promise` è costruito intorno al modulo Node.js `os.tmpdir()`, che fornisce un percorso sicuro per la creazione di file temporanei. La funzione `file()` restituisce un oggetto con due campi: un `path` e una funzione `cleanup()` per rimuovere il file temporaneo.

## Approfondisci 

1. Modulo [tmp-promise](https://www.npmjs.com/package/tmp-promise) su npm
2. Funzione Node.js [os.tmpdir()](https://nodejs.org/api/os.html#os_os_tmpdir)
3. Moduli alternativi: [mktemp](https://www.npmjs.com/package/mktemp), [tempfile](https://www.npmjs.com/package/tempfile)