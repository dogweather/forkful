---
title:                "Haskell: Verifica dell'esistenza di una directory"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Controllare l'esistenza di una directory è un'operazione fondamentale in programmazione Haskell. Ci permette di controllare se una specifica cartella esiste nel nostro sistema, e in caso contrario, di crearla per potervi scrivere o leggere file.

## Come Fare

Per controllare l'esistenza di una directory, possiamo utilizzare la funzione `doesDirectoryExist` del modulo `System.Directory`. Questa funzione prende come parametro una stringa contenente il percorso della directory che vogliamo controllare, e restituisce un valore booleano indicando se la directory esiste o meno.

```Haskell
import System.Directory

-- Definizione del percorso della directory
directoryPath = "C:\\Users\\utente\\Desktop\\documenti"

-- Chiamata alla funzione doesDirectoryExist
doesExist = doesDirectoryExist directoryPath

-- Stampa del risultato
main = do
    putStrLn $ "La directory " ++ directoryPath ++ " esiste? " ++ show doesExist
```

Questo codice restituirà il seguente output:

```
La directory C:\Users\utente\Desktop\documenti esiste? True
```

Se la directory non esiste, il risultato sarà `False`.

## Approfondimento

In Haskell, esistono diverse funzioni e librerie che ci permettono di controllare le directory in modo più approfondito. Ad esempio, con la funzione `listDirectory` del modulo `System.Directory`, possiamo ottenere una lista di tutte le sottodirectory presenti all'interno di una directory specifica.

```Haskell
import System.Directory

-- Definizione del percorso della directory
directoryPath = "C:\\Users\\utente\\Desktop\\documenti"

-- Chiamata alla funzione listDirectory
subDirectories = listDirectory directoryPath

-- Stampa del risultato
main = do
    putStrLn $ "Sottodirectories presenti in " ++ directoryPath ++ ": "
    mapM_ putStrLn subDirectories
```

Questo codice restituirà il seguente output:

```
Sottodirectories presenti in C:\Users\utente\Desktop\documenti:
foto
video
musiche
```

## Vedi Anche

- [Documentazione ufficiale di Haskell su System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Tutorial su come lavorare con le directory in Haskell](https://wiki.haskell.org/Working_with_files)
- [Domande frequenti su Haskell](https://wiki.haskell.org/FAQ)