---
title:                "Arrotondamento dei numeri"
date:                  2024-01-26T03:44:42.436807-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrotondamento dei numeri"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/rounding-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Arrotondare i numeri significa aggiustarli all'intero più vicino o al numero decimale specificato. I programmatori arrotondano i numeri per controllare la precisione, personalizzare gli output per la presentazione agli utenti, o ridurre i costi di calcolo per le operazioni in virgola mobile.

## Come fare:

Haskell utilizza le funzioni `round`, `ceiling`, `floor` e `truncate` dal `Prelude` per le operazioni di arrotondamento.

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- Arrotondare a un numero decimale specifico non è presente nel Prelude.
  -- Ecco qui una funzione personalizzata:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## Approfondimento

Storicamente, l'arrotondamento è significativo nell'analisi numerica e nell'informatica perché è cruciale per minimizzare l'accumulo di errori nei calcoli, in particolar modo prima che le rappresentazioni in virgola mobile fossero standardizzate con IEEE 754.

A cosa arrotondare? `round` ti porta all'intero più vicino—sia in su che in giù. `ceiling` e `floor` arrotondano sempre rispettivamente all'intero più vicino in su o in giù, mentre `truncate` semplicemente elimina i punti decimali.

Alternative a queste funzioni potrebbero implicare una logica personalizzata, come il nostro `roundTo`, o potresti incorporare librerie (come Data.Fixed) per requisiti più complessi.

Fai attenzione ai risultati inaspettati dovuti a come Haskell gestisce i casi a metà strada in `round` (arrotonda al numero pari più vicino).

## Vedi Anche

- Documentazione del Prelude di Haskell per le funzioni di arrotondamento: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- Il Wiki di Haskell sull'aritmetica in virgola mobile: https://wiki.haskell.org/Floating_point_arithmetic
- Lo standard IEEE 754-2008 per ulteriori informazioni su come viene gestita la virgola mobile in molte lingue: https://ieeexplore.ieee.org/document/4610935
