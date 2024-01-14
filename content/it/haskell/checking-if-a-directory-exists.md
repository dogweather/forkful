---
title:                "Haskell: Verificare se una directory esiste"
simple_title:         "Verificare se una directory esiste"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

La verifica dell'esistenza di una directory è un'operazione comune quando si lavora con file e cartelle all'interno di un progetto Haskell. Conoscere come effettuare questa verifica può aiutare a scrivere codice più robusto e gestire meglio eventuali errori.

## Come fare

Per verificare se una directory esiste, è necessario utilizzare la funzione `doesDirectoryExist` del pacchetto `System.Directory`.

```
-- Import del modulo System.Directory
import System.Directory
```

La funzione `doesDirectoryExist` accetta come argomento una stringa che rappresenta il percorso della directory che si vuole controllare e restituisce un booleano che indica se la directory esiste o meno.

```
-- Verifica se la directory "progetto" esiste
doesDirectoryExist "progetto"
-- Output: True
```

Se si desidera verificare l'esistenza di una directory relativa al percorso attuale, si può utilizzare la funzione `getCurrentDirectory` per ottenere il percorso corrente e concatenarlo con il nome della directory da controllare.

```
-- Ottenere il percorso corrente
currentDir <- getCurrentDirectory
-- Verifica se la directory "documenti" esiste nel percorso corrente
doesDirectoryExist (currentDir ++ "/documenti")
-- Output: False
```

## Approfondimento

La funzione `doesDirectoryExist` utilizza l'API di sistema operativo per verificare l'esistenza di una directory. Ciò significa che il suo comportamento potrebbe variare a seconda del sistema operativo su cui viene eseguita l'applicazione.

Inoltre, è possibile utilizzare la funzione `getPermissions` del modulo `System.Posix.Files` per ottenere informazioni aggiuntive sulle autorizzazioni di una directory. Questo può essere utile per controllare se l'utente ha i permessi necessari per accedere alla directory o modificarla.

```
import System.Posix.Files

-- Verifica se la directory "progetto" ha i permessi di scrittura
writeable <- writable <$> getPermissions "progetto"
-- Output: True
```

## Vedi anche

- [Documentazione completa della funzione `doesDirectoryExist`](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist)
- [Documentazione completa della funzione `getPermissions`](https://hackage.haskell.org/package/unix/docs/System-Posix-Files.html#v:getPermissions)
- [Tutorial su come gestire i file e le directory in Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/filesystem)