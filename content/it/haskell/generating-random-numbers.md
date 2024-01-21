---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:49:18.315580-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Generare numeri casuali è fondamentale in diversi campi, come crittografia, simulazione e giochi. In Haskell, lo facciamo per modellare eventi non deterministici o testare funzioni con dati variabili.

## How to: (Come fare:)
Haskell gestisce la casualità con puro funzionalismo; usa il modulo `System.Random`. Ecco come si fa:

```Haskell
import System.Random (newStdGen, randomRs)

-- Genera una lista infinita di numeri casuali interi:
randomInts :: Int -> IO [Int]
randomInts limit = do
  gen <- newStdGen
  return $ take limit $ randomRs (1, 100) gen

-- Uso della funzione:
main :: IO ()
main = do
  nums <- randomInts 10
  print nums
```

Esempio di output:

```
[23, 45, 95, 38, 76, 56, 89, 53, 60, 42]
```

## Deep Dive (In Profondità)
Il modulo `System.Random` esiste da molto, con Haskell98 che già aveva una specifica per i numeri casuali. Oggi, pacchetti come `random` e `tf-random` offrono più opzioni e performance migliori. Sotto il cofano, i numeri casuali in Haskell sono gestiti attraverso generatori pseudo-casuali (PRNG), che sono deterministici ma sembrano casuali se non conosci il seme (seed).

## See Also (Vedi Anche)
- Documentazione per il pacchetto [`random`](https://hackage.haskell.org/package/random)
- Documentazione per il pacchetto [`tf-random`](https://hackage.haskell.org/package/tf-random)