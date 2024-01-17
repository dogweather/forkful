---
title:                "Verificare l'esistenza di una directory"
html_title:           "Haskell: Verificare l'esistenza di una directory"
simple_title:         "Verificare l'esistenza di una directory"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Che cos'è e perché verificare se una directory esiste?
La verifica dell'esistenza di una directory è un'operazione che permette ai programmatori di verificare se una determinata directory è presente nel sistema operativo. Questa operazione è importante perché consente di assicurarsi che il programma possa accedere alle risorse necessarie per funzionare correttamente.

Come fare:
Di seguito sono riportati degli esempi di codice in Haskell per verificare se una directory esiste. Il primo metodo utilizza la funzione `doesDirectoryExist` del modulo `System.Directory`, mentre il secondo utilizza la funzione `doesPathExist` del modulo `System.FilePath`.

```Haskell
import System.Directory (doesDirectoryExist)
import System.FilePath (doesPathExist)

checkDirectory :: String -> IO Bool
checkDirectory path = doesDirectoryExist path

checkPath :: String -> IO Bool
checkPath path = doesPathExist path
```

Esempio di output:
```Haskell
> checkDirectory "Desktop"
True

> checkPath "Documents/file.txt"
True
```

Approfondimento:
La necessità di verificare l'esistenza di una directory è nata con l'avvento dei moderni sistemi operativi che supportano la gestione di file e cartelle. In passato, questa operazione non era necessaria poiché i file venivano memorizzati in un'unica directory.

Alternativamente, è possibile utilizzare la funzione `findDir` del modulo `System.Directory.Tree` per cercare una directory specifica all'interno di un'altra. Questo può essere utile quando si desidera eseguire determinate operazioni solo se la directory desiderata esiste.

Per quanto riguarda l'implementazione, la funzione `doesDirectoryExist` utilizza la chiamata di sistema `lstat` per controllare l'esistenza di una directory, mentre la funzione `doesPathExist` utilizza la chiamata di sistema `access`. Entrambe le funzioni restituiscono un valore booleano per indicare il risultato della verifica.

Vedi anche:
- Documentazione di Haskell su `System.Directory` e `System.FilePath`
- [Effettuare operazioni sulle directory in Haskell](https://www.codementor.io/@brett/how-to-check-for-file-directory-in-haskell-9la1czgvo)
- [Esempi pratici di utilizzo delle funzioni di verifica delle directory](https://www.reddit.com/r/haskell/comments/8vwm8y/finding_files_and_directories_func_files/)