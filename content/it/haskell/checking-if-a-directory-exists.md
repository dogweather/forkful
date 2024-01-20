---
title:                "Verifica se una directory esiste"
html_title:           "Haskell: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Controllare se esiste una Directory in Haskell
Un articolo per aiutare i programmatori italiani a navigare attraverso questa attività comune.

## Cosa & Perché?
Il controllo dell'esistenza di una directory significa verificare se una certa cartella esiste nella posizione specificata o meno. Quest'operazione permette ai programmatori di evitare errori durante l'esecuzione, garantendo che i dati acceduti o manipolati risiedano effettivamente in un percorso valido.

## Come Fare:
In Haskell, possiamo utilizzare la funzione `doesDirectoryExist` dal modulo `System.Directory` per controllare se una directory esiste. La funzione restituisce `True` se la directory esiste, e `False` altrimenti.

```haskell
import System.Directory

main :: IO ()
main = do
  let dir = "/percorso/alla/cartella"
  exists <- doesDirectoryExist dir
  if exists then putStrLn $ "La directory " ++ dir ++ " esiste." else putStrLn $ "La directory " ++ dir ++ " non esiste."
```

Se eseguiamo questo pezzo di codice, otterremo un'output simile a questo:

```shell
La directory /percorso/alla/cartella non esiste.
```

## Approfondimenti
La funzione `doesDirectoryExist` è disponibile in Haskell già dalla versione 1.0, pubblicata nel 2003. Essa fornisce un modo semplice ed efficiente per controllare l'esistenza di una directory, tuttavia ci sono alternative. Ad esempio, possiamo utilizzare la funzione `getDirectoryContents`, anch'essa del modulo `System.Directory`, per ottenere un elenco di tutte le cartelle e i file in una directory e poi cercare quella che ci interessa.

Tuttavia, questa soluzione è meno efficiente rispetto all'utilizzo di `doesDirectoryExist`, perché richiede il caricamento di un elenco completo di elementi della directory.

Funzioni come `doesDirectoryExist` sono fondamentali in qualsiasi linguaggio di programmazione, perché garantiscono che il codice interagisca solo con percorsi di file o directory validi. Siccome questi controlli sono effettuati al livello del sistema operativo, la loro implementazione precisi può variare a seconda della piattaforma su cui si esegue Haskell.

## Vedi Anche
1. [Modulo System.Directory - Hackage Haskell](https://hackage.haskell.org/package/directory-1.3.6.0/docs/System-Directory.html)