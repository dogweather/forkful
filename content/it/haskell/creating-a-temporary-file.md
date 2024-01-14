---
title:                "Haskell: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è una pratica comune in molte applicazioni di programmazione. Spesso, i programmatori utilizzano i file temporanei per memorizzare dati temporanei o effettuare operazioni su di essi. In questo post, esploreremo perché e come creare un file temporaneo utilizzando Haskell.

## Come fare

Per creare un file temporaneo in Haskell, è possibile utilizzare la funzione `withSystemTempFile` del modulo `System.IO.Temp`. Questa funzione prende come argomento una funzione che accetta il percorso e l'handle del file temporaneo e restituisce un risultato. Inoltre, è importante utilizzare la funzione `openTemporaryFile` per generare un nuovo file temporaneo.

Una volta creato il file temporaneo, è possibile leggerlo o scriverci dati utilizzando le funzioni standard di input/output di Haskell. Ad esempio, possiamo utilizzare le funzioni `hPutStrLn` e `hGetLine` per scrivere su e leggere da un file temporaneo.

Ecco un esempio di codice che crea un file temporaneo, vi scrive alcune stringhe e alla fine legge il contenuto del file:

```Haskell
import System.IO
import System.IO.Temp

withSystemTempFile "file.tmp" $ \path handle -> do
    hPutStrLn handle "Ciao!"
    hPutStrLn handle "Questo è un file temporaneo."
    hPutStrLn handle "È tempo di leggere il contenuto del file."
    hGetLine handle >>= putStrLn
```

L'output di questo codice sarà:

```
Ciao!
Questo è un file temporaneo.
È tempo di leggere il contenuto del file.
```

Si noti che il file temporaneo verrà automaticamente eliminato alla chiusura del blocco `withSystemTempFile`.

## Approfondimento

Quando si crea un file temporaneo in Haskell, è importante assicurarsi che il file sia accessibile solo al processo corrente. Ciò evita che altri processi possano accedere al file temporaneo mentre è in uso. Per fare ciò, è possibile utilizzare la funzione `setFileMode` del modulo `System.Posix`.

Inoltre, è possibile specificare un percorso specifico dove creare il file temporaneo, anziché affidarsi al sistema operativo per scegliere il percorso. Ciò può essere utile se si desidera salvare il file in una directory specifica o con un formato di nome predefinito.

## Vedi anche

- [Documentazione ufficiale di Haskell su come lavorare con i file](https://www.haskell.org/tutorial/io.html)
- [Esempi di utilizzo della funzione `withSystemTempFile`](https://rosettacode.org/wiki/Temporary_file_creation)