---
title:                "Haskell: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui potresti aver bisogno di convertire una stringa in minuscolo durante la programmazione Haskell. Ad esempio, potresti avere una stringa di input che devi confrontare con altre stringhe o semplicemente vuoi che l'output venga visualizzato in minuscolo. Indipendentemente dal motivo, conoscere come eseguire questa operazione può essere molto utile per rendere il tuo codice più flessibile e performante.

## Come fare

Per convertire una stringa in minuscolo in Haskell, puoi utilizzare la funzione `toLower` della libreria `Data.Char`. Ecco un esempio di codice:

```Haskell
import Data.Char (toLower)

main = do
    nome <- getLine
    let nomeMinuscolo = map toLower nome
    putStrLn nomeMinuscolo
```

Output:

```Haskell
> Simone
simone
```

## Approfondimento

La funzione `toLower` è in grado di gestire sia caratteri Unicode che ASCII. Se la stringa di input contiene caratteri speciali, questi verranno mantenuti nella loro forma minuscola. Inoltre, puoi utilizzare questa funzione anche per convertire solo una parte della stringa (ad esempio, una singola parola) anziché l'intera stringa. Puoi anche combinare la funzione `toLower` con altre funzioni per eseguire operazioni più complesse, come la rimozione di caratteri non alfanumerici o la formattazione del testo.

## Vedi anche

- [Tutorial su Haskell](https://hackage.haskell.org/packages/rdfs/docs/techinc/tutorial-Char.html)
- [Documentazione ufficiale di Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.14.1.0/Data-Char.html#v:toLower)
- [Esempi pratici di utilizzo di toLower](https://stackoverflow.com/questions/27478897/how-to-implement-a-function-that-makes-all-characters-in-a-string-lowercase-in)