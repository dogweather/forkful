---
title:                "Haskell: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error può essere utile quando si vuole comunicare informazioni importanti agli utenti durante l'esecuzione di un programma, come ad esempio messaggi di errore o di debug. Inoltre, scrivere su standard error è un buon modo per differenziare i messaggi importanti, da quelli che vengono scritti su standard output.

## Come Fare

Per scrivere su standard error in Haskell, è necessario importare il modulo "System.IO" e utilizzare la funzione "hPutStrLn" per scrivere una stringa su standard error. Ad esempio:

```Haskell
import System.IO

main = do
  hPutStrLn stderr "Questo è un messaggio di errore."
```

L'output del programma sarà:

```
Questo è un messaggio di errore.
```

In questo modo, la stringa viene scritta su standard error invece che su standard output.

## Approfondimento

Scrivere su standard error è utile quando si vuole separare i messaggi importanti, come i messaggi di errore, dai messaggi generici che vengono scritti su standard output. Inoltre, si può utilizzare la funzione "hPutStrLn" per scrivere qualsiasi tipo di stringa su standard error, inclusi messaggi di debug o di avviso. È importante ricordare di importare il modulo "System.IO" ogni volta che si vuole scrivere su standard error.

## Vedi Anche

- [Haskell Documentation](https://www.haskell.org/documentation/)
- [System.IO Module](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html)
- [Tutorial su Standard Output e Standard Error in Haskell](https://www.tutorialspoint.com/unix_commands/printf.htm)