---
title:    "Haskell: Lettura degli argomenti della riga di comando"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché

I comandi della linea di comando sono uno strumento essenziale per qualsiasi programmatore di Haskell. Con la loro potenza e flessibilità, consentono di interagire con il tuo programma in modo più dinamico e personalizzato. In questo articolo, esploreremo come leggere i comandi della linea di comando in Haskell e come possono essere utilizzati per migliorare il tuo codice.

## Come

La lettura dei comandi della linea di comando in Haskell è semplice e può essere fatta utilizzando la funzione `getArgs` del pacchetto `System.Environment`. Ecco un esempio di codice che legge il primo argomento della linea di comando e lo stampa a schermo:

```Haskell
import System.Environment (getArgs)

main = do
    args <- getArgs
    let firstArg = head args
    putStrLn ("Il primo argomento è: " ++ firstArg)
```

La funzione `getArgs` restituisce una lista di stringhe corrispondenti agli argomenti passati sulla linea di comando. Nel nostro esempio, utilizziamo la funzione `head` per ottenere il primo elemento della lista e lo stampiamo a schermo utilizzando la funzione `putStrLn`.

Ecco un esempio di come questo codice potrebbe essere eseguito e il suo output:

Input da linea di comando: `haskell reads_args_example.hs ciao mondo`

```
Il primo argomento è: ciao
```

## Deep Dive

Oltre alla funzione `getArgs` che abbiamo visto nell'esempio precedente, esiste anche la funzione `withArgs` che consente di eseguire una funzione specifica con gli argomenti forniti senza doverli passare nuovamente alla funzione `getArgs`. Ecco un esempio di codice che utilizza `withArgs` per ottenere l'ultimo argomento dalla linea di comando:

```Haskell
import System.Environment (getArgs, withArgs)

main = do
    let lastArg = withArgs ["ciao", "mondo"] getArgs
    putStrLn ("L'ultimo argomento è: " ++ lastArg)
```

L'output di questo codice sarà:

```
L'ultimo argomento è: mondo
```

## Vedi anche

- [Documentazione sulla funzione `getArgs`](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html#v:getArgs)
- [Documentazione sulla funzione `withArgs`](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html#v:withArgs)
- [Tutorial su come leggere e gestire i comandi della linea di comando in Haskell](https://wiki.haskell.org/Command_line_option_parsing)