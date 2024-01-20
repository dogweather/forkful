---
title:                "Scrivere sull'errore standard"
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error significa inviare messaggi di errore al flusso di errore standard, separato dall'output generale di un programma. I programmatori lo fanno per diagnosticare problemi senza intaccare l'output primario.

## How to:
```TypeScript
// Stampa un messaggio su standard error
console.error('Questo è un errore!');

// Uscita del programma con un codice di errore
process.exitCode = 1;
```
Output:
```
Questo è un errore!
```

## Deep Dive
Standard error, noto come `stderr`, è uno dei tre flussi di input/output standard introdotti nei primi sistemi Unix. È utilizzato principalmente per il logging degli errori. A differenza del logging standard (`standard output`, `stdout`), `stderr` non è bufferizzato. Questo significa che i messaggi di errore sono visualizzati immediatamente. Alternativamente, potresti scrivere su file di log o utilizzare librerie di log esterne per una gestione più sofisticata degli errori.

## See Also
- Node.js documentation on process.stderr: [Node.js process.stderr](https://nodejs.org/api/process.html#process_process_stderr)