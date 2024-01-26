---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-20T14:56:48.730114-07:00
html_title:           "Gleam: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Verificare l'esistenza di una directory significa assicurarsi che un percorso specificato corrisponda a una cartella sul file system. I programmatori lo fanno per evitare errori nei programmi, come tentare di accedere o scrivere in una cartella inesistente.

## How to:
In Haskell, usiamo il modulo `System.Directory` per controllare l'esistenza di una directory. Ecco un esempio:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/path/to/directory"
  dirExists <- doesDirectoryExist dirPath
  putStrLn $ "La directory " ++ (if dirExists then "esiste." else "non esiste.")
```

Esempio di output se la directory esiste:

```
La directory esiste.
```

E se non esiste:

```
La directory non esiste.
```

## Deep Dive
Il modulo `System.Directory` è parte della libreria base di Haskell e fornisce funzionalità di interazione con il file system. Introdotta nei primi anni del linguaggio, questa funzionalità aiuta a evitare errori comuni di I/O.

Come alternativa, potresti voler esplorare il pacchetto `directory`, che offre una gamma più ampia di operazioni sui filesystem. Implementare la verifica di una directory senza `doesDirectoryExist` può comportare il controllo degli errori di I/O basso livello, ma questo è sconsigliato a meno che non sia strettamente necessario.

## See Also
- Per ulteriori informazioni sulla libreria `System.Directory`, consulta [Hackage - System.Directory](https://hackage.haskell.org/package/directory)
- Tutorial Haskell generale: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- Specifiche della libreria base Haskell: [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/)
