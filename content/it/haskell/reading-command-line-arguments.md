---
title:                "Lettura degli argomenti della linea di comando"
html_title:           "Haskell: Lettura degli argomenti della linea di comando"
simple_title:         "Lettura degli argomenti della linea di comando"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Oggi impareremo come leggere gli argomenti della riga di comando utilizzando Haskell. Questa abilità ti permetterà di creare programmi più dinamici e interattivi in ​​linea di comando.

## Come fare

Leggere gli argomenti della riga di comando in Haskell è semplicemente una questione di utilizzare una funzione fornita dalla libreria standard, chiamata `getArgs`. Ecco un esempio di come utilizzarla:

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn ("Hai inserito " ++ show (length args) ++ " argomenti: " ++ show args)
```

In questo esempio, abbiamo importato il modulo `System.Environment`, che ci fornisce la funzione `getArgs`. Nella nostra funzione `main`, chiamiamo `getArgs` e salviamo il risultato nella variabile `args`. Quindi, utilizziamo la funzione `putStrLn` per stampare a schermo il numero di argomenti inseriti e una rappresentazione degli argomenti stessi. Ecco un esempio di output:

```
$ runhaskell args.hs uno due tre
Hai inserito 3 argomenti: ["uno","due","tre"]
```

Come puoi vedere, `getArgs` restituisce una lista di stringhe corrispondenti agli argomenti che abbiamo inserito al momento dell'esecuzione del programma.

## Approfondimento

È importante notare che la funzione `getArgs` non solo legge gli argomenti dalla riga di comando, ma anche dalla variabile di ambiente `args`. Ciò significa che puoi passare gli argomenti al tuo programma anche tramite la variabile di ambiente, in modo da poter testare il tuo programma senza doverlo eseguire manualmente dalla riga di comando ogni volta.

Puoi anche utilizzare la funzione `System.Environment.withArgs` per eseguire una parte del codice con una lista personalizzata di argomenti, invece che utilizzare quelli passati dalla riga di comando. Questo può essere utile per i test.

## Vedi anche

- Documentazione ufficiale di `getArgs`: https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html#v:getArgs
- Tutorial su come utilizzare le variabili di ambiente in Haskell: https://www.tweag.io/blog/2019-01-10-env-vars/