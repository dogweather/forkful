---
title:                "Verifica dell'esistenza di una directory"
html_title:           "Haskell: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Se stai programmando in Haskell, potresti trovarti nella situazione di dover verificare se una specifica directory esiste o meno all'interno del tuo sistema operativo. Ciò potrebbe essere utile per assicurarti di avere il giusto percorso di directory per una determinata operazione o per gestire possibili errori.

## Come fare

Per verificare se una directory esiste in Haskell, puoi utilizzare la funzione `doesDirectoryExist` dal modulo `System.Directory`. Questa funzione accetta come parametro un percorso di directory e restituisce un valore booleano che indica se la directory esiste o meno.

```Haskell
import System.Directory
 
main = do
  dirExists <- doesDirectoryExist "path/to/directory"
  if dirExists
    then putStrLn "La directory esiste!"
    else putStrLn "La directory non esiste."
```

Il codice sopra mostra un esempio di come utilizzare la funzione `doesDirectoryExist` per controllare se una directory esiste. Nota che il percorso della directory deve essere specificato tra virgolette per indicare una stringa.

## Approfondimento

Alcune cose da tenere in considerazione quando si utilizza la funzione `doesDirectoryExist` in Haskell sono:

- La funzione può lanciare un'eccezione se il passaggio del percorso di directory viene eseguito in modo errato.
- La funzione non fa distinzione tra una directory effettiva e un link simbolico che punta a una directory esistente. Se è necessario distinguere tra i due, è necessario utilizzare la funzione `getSymbolicLinkStatus` dal modulo `System.Posix.Files`.
- Se si ha bisogno di effettuare operazioni sulla directory dopo aver verificato la sua esistenza, è consigliabile utilizzare la funzione `withCurrentDirectory` dal modulo `System.Directory`, che eseguirà le operazioni all'interno della directory specificata.

## Vedi anche

- Funzione `createDirectory` - per creare una nuova directory in Haskell: https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:createDirectory
- Modulo `System.Posix.Files` - per ulteriori funzioni per lavorare con file e directory in Haskell: https://hackage.haskell.org/package/unix/docs/System-Posix-Files.html
- Documentazione su Haskell - per imparare di più su questo linguaggio di programmazione funzionale: https://www.haskell.org/documentation/