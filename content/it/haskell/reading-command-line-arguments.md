---
title:    "Haskell: Leggere gli argomenti della riga di comando"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Leggere gli argomenti della riga di comando è un'abilità fondamentale per qualsiasi programmatore Haskell. Con questa conoscenza, è possibile rendere i tuoi programmi più flessibili e interattivi.

## Come

Per leggere gli argomenti della riga di comando in Haskell, useremo la funzione `getArgs` dal modulo `System.Environment`. Questa funzione restituisce una lista di stringhe contenenti gli argomenti forniti al programma nella riga di comando.

Ad esempio, se eseguiamo il seguente programma:

```Haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    print args
```

Con il comando `runhaskell myprogram.hs arg1 arg2`, otterremo il seguente output:

```Haskell
["arg1","arg2"]
```

In questo caso, la lista contiene due stringhe: `"arg1"` e `"arg2"`, che corrispondono agli argomenti della riga di comando forniti al programma.

## Deep Dive

Oltre alla funzione `getArgs`, esistono altre opzioni per leggere gli argomenti della riga di comando in Haskell. Per esempio, possiamo utilizzare la funzione `withArgs` dal modulo `System.Console.GetOpt` per analizzare gli argomenti e gestire eventuali errori.

Inoltre, è possibile specificare opzioni e argomenti opzionali nella riga di comando utilizzando la libreria `optparse-applicative`. Questo rende il processo di lettura degli argomenti più strutturato e facilmente gestibile.

## See Also

- Documentazione ufficiale di `System.Environment`: https://hackage.haskell.org/package/base/docs/System-Environment.html
- Documentazione ufficiale di `System.Console.GetOpt`: https://hackage.haskell.org/package/base/docs/System-Console-GetOpt.html
- Documentazione ufficiale di `optparse-applicative`: https://hackage.haskell.org/package/optparse-applicative