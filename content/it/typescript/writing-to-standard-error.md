---
title:                "Scrivere su standard error"
html_title:           "TypeScript: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Scrivere su standard error (stderr) è un metodo utilizzato dai programmatori per inviare messaggi di errore o di debug al terminale. Questo è utile perché i messaggi su stderr sono visualizzati immediatamente e possono aiutare a diagnosticare e risolvere problemi durante l'esecuzione del programma.

## Come:

```TypeScript
console.error("Errore critico!");
```

**Output:**

```
Errore critico!
```

## Approfondimento:

La scrittura su stderr è una pratica comune nei linguaggi di programmazione, in particolare nei sistemi operativi Unix. Nella maggior parte dei casi, stderr è collegato al terminale, quindi i messaggi saranno visualizzati direttamente senza dover essere cercati in altre pagine di output.

Un'alternativa alla scrittura su stderr è l'utilizzo di console.log(), che invia i messaggi su stdout (standard output), ma i programmatori preferiscono spesso stderr per evitare che i messaggi di debug si confondano con l'output normale del programma.

In TypeScript, è possibile utilizzare anche il modulo "process" per accedere a stderr. Ad esempio:

```TypeScript
import * as process from 'process';

process.stderr.write("Errore critico!");
```

## Vedi anche:

- Documentazione ufficiale TypeScript su console.error(): https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-1.html#console-error-with-no-message-type-error-fix
- Tutorial su come utilizzare console.error() in TypeScript: https://www.hackdoor.io/articles/console-error--how-to-write-to-standard-error-in-typescript-31lw0RNA