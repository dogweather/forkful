---
title:    "Haskell: Scrivere un file di testo"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo può sembrare un compito banale, ma in realtà ha molti benefici. Ad esempio, un file di testo può essere utilizzato per memorizzare dati o configurazioni, risparmiando tempo e sforzo nella riscrittura ogni volta che si utilizza il programma.

## Come fare

Per scrivere un file di testo in Haskell, è necessario importare il modulo "System.IO". Utilizzando la funzione "withFile", possiamo specificare il nome del file, la modalità di apertura e l'azione da eseguire all'interno del file. Vediamo un esempio:

```Haskell
import System.IO

main = do
  let filename = "testfile.txt" -- nome del file
  let mode = WriteMode -- modalità di apertura
  withFile filename mode (\handle -> do -- apriamo il file e definiamo una funzione di callback
    hPutStrLn handle "Ciao a tutti!" -- scriviamo una stringa nel file
    hClose handle -- chiudiamo il file
  )
```

L'output di questo codice è un file chiamato "testfile.txt" contenente il messaggio "Ciao a tutti!".

## Approfondimento

Oltre alla semplice scrittura di testo, Haskell offre diverse funzioni utili per lavorare con i file di testo. Ad esempio, si possono leggere i contenuti di un file con la funzione "readFile" o ottenere il nome del file corrente con la funzione "getProgName". Inoltre, è possibile gestire errori con la funzione "try", che restituisce un valore di tipo "Either" a seconda del successo o del fallimento dell'azione eseguita sul file.

## Vedi anche
- [Haskell - Gestione dei file di testo](https://www.haskell.org/tutorial/io.html)
- [Haskell - Documentazione del modulo System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Haskell - Gestione degli errori con "Either"](https://www.haskell.org/tutorial/errormsgs.html)