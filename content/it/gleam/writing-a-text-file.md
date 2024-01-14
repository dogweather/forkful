---
title:    "Gleam: Scrivere un file di testo"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

La scrittura di un file di testo è fondamentale per la programmazione in Gleam. Questa operazione consente di salvare e gestire i dati in modo organizzato, rendendo più semplice la loro manipolazione da parte del programma.

## Come fare

Per scrivere un file di testo in Gleam, è necessario utilizzare la libreria standard `gleam/io` e il modulo `File`. Di seguito è riportato un esempio di codice che mostra come creare un nuovo file di testo chiamato "mionuovofile.txt" e scrivere il testo "Ciao Mondo!" al suo interno.

```Gleam
import gleam/io
import gleam/stdlib/io/file

fn write_text() {
  let file = File.create("mionuovofile.txt")  // crea un nuovo file
  File.write(file, "Ciao Mondo!")  // scrive "Ciao Mondo!" nel file
}

main() {
  write_text()  // chiama la funzione per scrivere il testo
  ok // segnala che il programma è stato eseguito con successo
}
```

L'output di questo codice sarà un nuovo file di testo chiamato "mionuovofile.txt" con il contenuto "Ciao Mondo!".

## Approfondimento

Scrivere un file di testo non è solo una questione di scrivere un testo "hardcoded" all'interno del codice. È possibile anche leggere il testo da un'API esterna o creare un file di testo con dati dinamici. Inoltre, la libreria `gleam/stdlib/io/file` fornisce anche altre funzioni utili per la gestione dei file di testo, come ad esempio la lettura e la cancellazione di un file.

## Vedi anche

- Documentazione su File in Gleam: https://gleam.run/modules/gleam/stdlib/io/file/
- Tutorial su I/O in Gleam: https://dev.to/talkblastbaby/getting-started-with-io-in-gleam-2af3