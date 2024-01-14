---
title:    "Haskell: Verifica dell'esistenza di una directory"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché?

Spesso, nelle nostre attività di programmazione, abbiamo la necessità di verificare l'esistenza di una determinata directory prima di procedere con le operazioni su di essa. Questo è particolarmente utile quando lavoriamo con file o database memorizzati all'interno di una directory specifica.

## Come fare?

Per verificare l'esistenza di una directory in Haskell, possiamo utilizzare la funzione `doesDirectoryExist` del modulo `System.Directory`. Questa funzione accetta come argomento una stringa che rappresenta il percorso della directory da verificare e restituisce un valore booleano che indica se la directory esiste o meno.

```Haskell
import System.Directory
main = do
  exists <- doesDirectoryExist "path/to/directory"
  if exists
    then putStrLn "La directory esiste!"
    else putStrLn "La directory non esiste!"
```

Nell'esempio sopra, utilizziamo la sintassi `do` per eseguire le operazioni all'interno di un blocco sequenziale. La funzione `putStrLn` viene utilizzata per stampare un messaggio a schermo. L'operatore `<-` viene utilizzato per estrarre il valore restituito dalla funzione `doesDirectoryExist`.

## Approfondimento

La funzione `doesDirectoryExist` utilizza il sistema operativo sottostante per verificare l'esistenza della directory. Se la directory non esiste, la funzione restituisce `False`. Tuttavia, se la directory esiste ma non è possibile accedervi per motivi di sicurezza o autorizzazioni, la funzione restituirà comunque `True`. Inoltre, non è possibile verificare l'esistenza di una directory all'interno di una directory "virtuale" creata durante l'esecuzione del programma.

## Vedi anche

- Tutorial su Haskell: https://www.haskell.org/
- Modulo System.Directory: https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html