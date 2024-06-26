---
date: 2024-01-26 01:18:30.339242-07:00
description: "Come fare: Diciamo che hai un blocco di codice Haskell che si ripete\
  \ pi\xF9 della tua canzone preferita. Ecco un rapido sguardo su come potresti\u2026"
lastmod: '2024-03-13T22:44:43.483538-06:00'
model: gpt-4-0125-preview
summary: "Diciamo che hai un blocco di codice Haskell che si ripete pi\xF9 della tua\
  \ canzone preferita."
title: Rifattorizzazione
weight: 19
---

## Come fare:
Diciamo che hai un blocco di codice Haskell che si ripete più della tua canzone preferita. Ecco un rapido sguardo su come potresti rifattorizzarlo usando le funzioni.

Prima del refactoring:

```haskell
printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  putStrLn $ "Customer: " ++ customer
  putStrLn $ "Total: " ++ show total
  putStrLn $ "Item: " ++ item
```

Dopo un po' di refactoring:

```haskell
printDetail :: String -> String -> IO ()
printDetail label value = putStrLn $ label ++ ": " ++ value

printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  printDetail "Customer" customer
  printDetail "Total" (show total)
  printDetail "Item" item

-- Esempio di output:
-- Customer: Alice
-- Total: $42.00
-- Item: Guida alla Programmazione Haskell
```

Come puoi vedere, estraendo il modello comune in una funzione `printDetail` separata, evitiamo ripetizioni e rendiamo `printInvoice` più chiaro e facile da gestire.

## Approfondimento
Quando Haskell è arrivato sulla scena alla fine degli anni '80, era chiaro che il paradigma funzionale poteva portare una ventata di aria fresca nelle pratiche di codifica. Andando avanti veloce, il refactoring in Haskell è particolarmente elegante grazie al fatto che le funzioni sono cittadini di prima classe e al suo forte sistema di tipi statici. Puoi rifattorizzare senza temere di rompere la tua app, poiché il compilatore copre le tue spalle.

Le alternative al refactoring manuale possono includere l'uso di strumenti automatizzati, anche se la natura funzionale e la sicurezza dei tipi di Haskell possono a volte renderlo meno prevalente rispetto ad altre lingue. A livello di implementazione, è importante sfruttare le caratteristiche di Haskell come le funzioni di ordine superiore, la purezza e l'immutabilità per rendere il refactoring più fluido.

Rifattorizzazioni come "Estrarre Funzione", appena mostrato, sono comuni, ma puoi anche fare "Inserire Funzione", "Rinominare Variabile" e "Cambiare Firma della Funzione" con fiducia, grazie al sistema di tipi. L'inferenza di tipo potente di Haskell a volte può catturare errori che scivolerebbero attraverso in altre lingue.

## Vedi Anche
Per un approfondimento sul refactoring in Haskell, consulta il libro "Refactoring: Improving the Design of Existing Code" di Martin Fowler, dove i concetti sono universalmente applicabili. Controlla lo strumento hlint per suggerimenti automatizzati su come migliorare il tuo codice Haskell. Visita anche il wiki di Haskell (https://wiki.haskell.org/Refactoring) per intuizioni della comunità e ulteriori letture.
