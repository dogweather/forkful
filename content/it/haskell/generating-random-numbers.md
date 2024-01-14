---
title:                "Haskell: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

La generazione di numeri casuali è un'attività comune e utile nella programmazione. Può essere utilizzata per creare giochi, simulazioni o per testare algoritmi. Inoltre, imparare a generare numeri casuali in Haskell ci permette di esplorare le funzionalità del linguaggio e arricchire le nostre capacità di programmazione.

## Come si fa

In Haskell, la generazione di numeri casuali è possibile grazie al modulo `System.Random`, che fornisce una serie di funzioni per creare e manipolare generatori di numeri casuali.

```Haskell
import System.Random

-- Creare un generatore di numeri casuali a partire da un valore seed
mkStdGen :: Int -> StdGen

-- Generare un numero intero casuale nell'intervallo specificato
randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)

-- Generare una lista di numeri casuali interi
randomRs :: (Random a, RandomGen g) => (a, a) -> g -> [a]
```

Utilizzando `mkStdGen`, possiamo creare un generatore di numeri casuali fornendo un valore di seed. Successivamente, utilizzando `randomR` o `randomRs` possiamo generare uno o più numeri casuali rispettivamente. Ecco un esempio di codice che genera tre numeri casuali tra 1 e 10:

```Haskell
import System.Random

main = do
    let gen = mkStdGen 10 -- creiamo un generatore di numeri casuali con seed 10
        (num1, gen') = randomR (1, 10) gen -- generiamo il primo numero casuale
        (num2, gen'') = randomR (1, 10) gen' -- generiamo il secondo numero casuale
        (num3, _) = randomR (1, 10) gen'' -- generiamo il terzo numero casuale
    putStrLn $ show num1 -- stampiamo i numeri casuali
    putStrLn $ show num2
    putStrLn $ show num3
```

L'output di questo programma potrebbe essere:

```
5
3
9
```

Notiamo che ogni volta che viene chiamata una funzione di generazione di numeri casuali, viene restituito anche un nuovo generatore che può poi essere utilizzato per generare ulteriori numeri.

## Approfondimento

In Haskell, la generazione di numeri casuali è basata sull'algoritmo di generazione di numeri casuali Mersenne Twister, un algoritmo probabilistico molto efficiente e ampiamente utilizzato.

Inoltre, è possibile manipolare i generatori di numeri casuali in modi più avanzati, ad esempio combinandoli tra loro o utilizzando la funzione `split` per dividerne uno in due nuovi generatori.

La generazione di numeri casuali può anche essere utilizzata per implementare algoritmi di generazione di sequenze pseudo-casuali, utili in contesti come la crittografia.

## Vedi anche

- [Documentazione del modulo `System.Random` su Hackage](https://hackage.haskell.org/package/random/docs/System-Random.html)
- [Tutorial su come generare numeri casuali in Haskell](https://www.codementor.io/@sheena/haskell-random-number-generation-tutorial-cguedb1at)
- [Mersenne Twister su Wikipedia](https://it.wikipedia.org/wiki/Mersenne_Twister)