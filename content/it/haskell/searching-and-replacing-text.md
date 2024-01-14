---
title:    "Haskell: Ricerca e sostituzione di testo"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché

La ricerca e la sostituzione di testo sono attività comuni durante la scrittura di codice. In un mondo in cui il tempo è prezioso, imparare a farlo in modo efficiente può risparmiare molto tempo e sforzi.

## Come fare

Per eseguire una ricerca e sostituzione di testo in Haskell, è necessario utilizzare la funzione `replace` del modulo `Data.List`.

```Haskell
import Data.List (replace)

replace "vecchio" "nuovo" "Questa è una vecchia stringa."
```

Questo restituirà la stringa "Questa è una nuova stringa." come output. La funzione `replace` accetta tre argomenti: la sottostringa da sostituire, la nuova sottostringa e la stringa originale.

Puoi anche utilizzare la funzione `replaceAll` per sostituire tutte le occorrenze della sottostringa nella stringa originale.

```Haskell
import Data.List (replaceAll)

replaceAll "a" "e" "banana"
```

Questo restituirà "benene" come output. In questo caso, tutte le occorrenze della lettera "a" sono state sostituite con la lettera "e".

## Approfondimento

La funzione `replace` e `replaceAll` sono entrambe basate sull'uso della funzione `isPrefixOf` del modulo `Data.List`. Questa funzione restituisce `True` se la sottostringa è un prefisso della stringa di input.

Per esempio, `isPrefixOf "abc" "abcdefg"` restituirà `True`.

Questa funzione è utile per capire come avviene la sostituzione di una sottostringa all'interno di una stringa.

## Vedi anche

- [Haskell Wiki - Modulo Data.List](https://wiki.haskell.org/Data.List)
- [Documentazione di Haskell per la funzione `replace`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#g:9)
- [Guida di riferimento per Haskell](https://wiki.haskell.org/Reference_card)