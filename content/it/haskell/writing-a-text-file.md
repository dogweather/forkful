---
title:                "Scrivere un file di testo"
date:                  2024-01-19
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere un file di testo significa salvare dati in un formato leggibile. Programmatori lo fanno per persistenza dei dati, configurazioni, o per la generazione di report.

## How to:
Per scrivere un file usiamo `writeFile` o `appendFile`:

```Haskell
import System.IO

-- Scrivi un nuovo file o sovrascrivi uno esistente
main :: IO ()
main = do
    let contenuto = "Ciao, questo è un testo in un file!"
    writeFile "esempio.txt" contenuto
```

Se esegui e guardi `esempio.txt`, troverai il testo. Per aggiungere al file, senza sovrascrivere:

```Haskell
-- Aggiunge contenuto al file esistente
main :: IO ()
main = do
    let nuovoContenuto = "\nAggiungi questa nuova linea al file."
    appendFile "esempio.txt" nuovoContenuto
```

## Deep Dive
Haskell ha introdotto `writeFile` e `appendFile` negli anni '90. Alternativamente, puoi usare `hPutStr` con il file aperto in modalità scrittura. Per performance, considera `hPutStr` con `Handle` quando hai scritture ripetute.

Esempio con `hPutStr`:

```Haskell
main :: IO ()
main = do
    handle <- openFile "esempio.txt" WriteMode
    hPutStr handle "Usando hPutStr per scrivere nel file."
    hClose handle
```

## See Also
- [Haskell Documentation for IO](https://hackage.haskell.org/package/base/docs/Prelude.html#g:25)
- [Real World Haskell, Chapter 7: I/O](http://book.realworldhaskell.org/read/io.html)
