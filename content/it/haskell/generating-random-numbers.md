---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generatore di Numeri Casuali in Haskell

## Cos'è & Perché?

Generare numeri casuali significa creare una sequenza di numeri senza logica apparente. In programmazione, usiamo questa tecnica per eventi imprevedibili, simulazioni e, a volte, testing.

## Come fare:

In Haskell, il modulo `System.Random` rende semplice la generazione di numeri casuali.

```Haskell 
import System.Random

main = do
    numeroCasuale <- randomRIO (0, 100) :: IO Int
    print numeroCasuale
```

Ecco un esempio di output:

```
58
```

## Approfondimento

1. Contesto storico: Haskell ha un ricco ecosistema di librerie per la generazione di numeri casuali. Il modulo `System.Random`, introdotto negli anni '90, rimane uno dei più utilizzati.

2. Alternative: Un'alternativa popolare a `System.Random` è la libreria `mwc-random`, conosciuta per la sua velocità. `random-fu` è un'altra opzione che offre un'interfaccia più funzionale.

3. Dettagli implementativi: Haskell genera numeri casuali in maniera diversa rispetto ad altri linguaggi di programmazione. Usa un generatore di numeri pseudocasuali - una funzione che produce una successione di valori che appaiono casuali.

## Per saperne di più

Per ulteriori informazioni, si prega di visitare le seguenti risorse:

1. Documentazione ufficiale di `System.Random`: [https://hackage.haskell.org/package/random-1.1/docs/System-Random.html](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
2. Libreria `mwc-random`: [https://hackage.haskell.org/package/mwc-random](https://hackage.haskell.org/package/mwc-random)
3. Libreria `random-fu`: [https://hackage.haskell.org/package/random-fu](https://hackage.haskell.org/package/random-fu)