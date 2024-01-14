---
title:                "Haskell: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo è una delle attività più comuni che si possono fare con il linguaggio di programmazione Haskell. Questo può essere utile per leggere input da un utente, caricare dati predefiniti o manipolare file di testo esistenti. In questo post, esploreremo come leggere un file di testo utilizzando Haskell e come approfondire ulteriormente questa operazione.

## Come fare

Per leggere un file di testo in Haskell, possiamo utilizzare la funzione `readFile` della libreria standard `System.IO`. Questa funzione prende come input il percorso del file e restituisce una `IO String` che rappresenta il contenuto del file. Iniziamo creando un file di testo chiamato "test.txt" con alcuni contenuti all'interno.

```
Ciao! Questo è un file di testo.
```

Per leggere questo file dentro il nostro codice Haskell, possiamo utilizzare il seguente snippet:

```haskell
import System.IO

main = do
  contenuto <- readFile "test.txt"
  putStrLn contenuto
```

Questo codice importa la libreria `System.IO` e usa la funzione `readFile` per leggere il file "test.txt" e assegnare il suo contenuto alla variabile `contenuto`. Successivamente, stampiamo il contenuto utilizzando la funzione `putStrLn` che è inclusa nella libreria standard `Prelude`.

Alcune cose da notare in questo esempio: la funzione `readFile` ritorna una `IO String` perché leggere un file può comportare effetti laterali (come l'accesso al filesystem). Ecco perché dobbiamo utilizzare la monade `IO`. Inoltre, il contenuto del file viene restituito come un unico stringa, quindi dovremmo utilizzare funzioni della libreria standard per manipolarlo (ad esempio, `lines` per ottenere una lista di righe o `words` per ottenere una lista di parole).

## Approfondimento

Ora che sappiamo come leggere un file di testo in Haskell, possiamo approfondire ulteriormente questa operazione attraverso alcune funzioni aggiuntive. La libreria standard `System.IO` offre diverse altre funzioni utili per manipolare file, tra cui `openFile` per aprire un file specifico in una modalità specifica (lettura, scrittura, etc.) e `getContents` per leggere l'intero contenuto di un file senza la necessità di specificare il percorso. Inoltre, possiamo utilizzare la funzione `withFile` per assicurarci che il file venga correttamente chiuso dopo essere stato utilizzato.

Al di fuori della libreria standard, esistono anche librerie di terze parti che offrono funzionalità più avanzate per la lettura dei file, come ad esempio `text`, che offre funzioni di parsing e manipolazione di testo efficienti. Se stiamo lavorando con file di grandi dimensioni, è consigliato utilizzare librerie come questa per ottenere prestazioni migliori.

## Vedi anche

- [Haskell documentation](https://www.haskell.org/documentation/)
- [Real World Haskell](http://book.realworldhaskell.org/read/io.html)
- [Haskell Text Processing](https://wiki.haskell.org/Text_Processing)