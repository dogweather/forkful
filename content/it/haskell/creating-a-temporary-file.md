---
title:                "Creazione di un file temporaneo"
html_title:           "Haskell: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Creare un file temporaneo è un'operazione comune in programmazione. 
I programmatori lo fanno per salvare temporaneamente dati, configurazioni o altri tipi di informazioni durante l'esecuzione del programma. 

## Come fare:

```Haskell
import System.IO

main = do
    -- creare un nuovo file temporaneo
    tempFile <- openTempFile "path" "prefix"

    -- scrivere del contenuto nel file
    hPutStr tempFile "Questo è un file temporaneo."

    -- chiudere il file temporaneo
    hClose tempFile

    -- ottenere il contenuto del file temporaneo
    tempContent <- readFile (fst tempFile)
    putStrLn tempContent

    -- eliminare il file temporaneo
    removeFile (fst tempFile)
```

Output:

```sh
Questo è un file temporaneo.
```

## Approfondimento:

Creare un file temporaneo è un'operazione che ha un'importante storia dietro di sé, poiché è stato introdotto per evitare perdita di dati in caso di crash del sistema. 
Un'alternativa al file temporaneo è l'utilizzo di una memoria temporanea, come ad esempio la memoria RAM. 
L'implementazione di un file temporaneo dipende dal sistema operativo utilizzato e può comportare la creazione di un vero e proprio file o l'utilizzo di memoria virtuale.

## Vedi anche:

- Riferimento ufficiale per la gestione dei file temporanei in Haskell: https://hackage.haskell.org/package/temporary
- Altro approfondimento sull'utilizzo dei file temporanei: https://wiki.haskell.org/Temporary\_files
- Utilizzo di memoria temporanea in Haskell: https://hackage.haskell.org/package/hs-memory