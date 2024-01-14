---
title:                "Gleam: Lettura di un file di testo"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

In questo post, parleremo di come leggere un file di testo utilizzando il linguaggio di programmazione Gleam. Leggere un file di testo è una competenza fondamentale per qualsiasi programmatore e ti permette di accedere ai dati salvati in un file.

## Come

Per leggere un file di testo in Gleam, possiamo utilizzare la funzione ```Gleam.file.read``` seguita dal percorso del file che vogliamo leggere come argomento. Ad esempio, se vogliamo leggere un file chiamato "dati.txt" situato nella stessa cartella del nostro programma, possiamo utilizzare il seguente codice:

```Gleam
let dati = Gleam.file.read("dati.txt")
```

Questo codice leggerà il contenuto del file e lo salverà nella variabile ```dati```. Possiamo poi utilizzare questa variabile per manipolare i dati all'interno del file.

## Deep Dive

Quando leggiamo un file di testo, il contenuto viene letto come una stringa. Possiamo utilizzare le funzioni di manipolazione delle stringhe di Gleam per accedere ai singoli caratteri o per separare il contenuto in base a un carattere specifico. Inoltre, possiamo anche gestire eventuali errori che potrebbero verificarsi durante la lettura del file.

Ad esempio, se vogliamo leggere una linea del file alla volta, possiamo utilizzare la funzione ```String.split``` per separare il contenuto in base a un carattere di nuova riga e poi utilizzare un loop per accedere a ogni linea. In questo modo, possiamo elaborare le informazioni all'interno del file in modo efficiente.

## Vedi anche

- [Documentazione ufficiale di Gleam](https://gleam.run)
- [Tutorial su come scrivere un file di testo in Gleam](https://url2it.com/laznpq)