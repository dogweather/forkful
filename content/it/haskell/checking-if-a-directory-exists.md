---
title:    "Haskell: Verifica dell'esistenza di una directory"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Spesso durante la programmazione potremmo trovarci nella situazione in cui abbiamo bisogno di verificare se una directory esiste o meno. Questo può essere utile per garantire che il nostro programma funzioni correttamente e per gestire possibili errori.

## Come fare

Per verificare se una directory esiste in Haskell, possiamo utilizzare la funzione `doesDirectoryExist` presente nel modulo `System.Directory`. Questa funzione prende come argomento il percorso della directory e restituisce un valore booleano, True se la directory esiste, False altrimenti.

Ecco un semplice esempio di come utilizzare questa funzione:

```Haskell
import System.Directory (doesDirectoryExist)

main = do
    result <- doesDirectoryExist "Documents"
    if result
        then putStrLn "La directory esiste!"
        else putStrLn "La directory non esiste."
```

Nell'esempio, utilizziamo la funzione `doesDirectoryExist` per controllare se la directory "Documents" esiste nel nostro sistema. Se la directory esiste, stampiamo un messaggio di conferma, altrimenti stampiamo un messaggio di errore.

## Approfondimento

Oltre alla funzione `doesDirectoryExist`, il modulo `System.Directory` mette a disposizione altre utili funzioni per manipolare le directory. Ad esempio, possiamo utilizzare la funzione `getDirectoryContents` per ottenere una lista dei file e delle directory presenti all'interno di una determinata directory. Inoltre, possiamo utilizzare la funzione `createDirectory` per creare una nuova directory e la funzione `removeDirectoryRecursive` per rimuovere una directory e tutti i suoi contenuti.

## Vedi anche

- Documentazione ufficiale di Haskell su System.Directory: https://www.haskell.org/cabal/users-guide/installing-packages.html
- Una guida dettagliata sull'utilizzo delle directory in Haskell: https://en.wikibooks.org/wiki/Haskell/directory
- Esempi di codice per gestire le directory in Haskell: https://github.com/fpco/haskell-ide-engine/blob/master/hie-plugin-api/src/Util.hs