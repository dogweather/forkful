---
title:    "Haskell: Eliminazione di caratteri corrispondenti a un pattern"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Quando si scrive codice in Haskell, può capitare di trovarsi in situazioni in cui è necessario eliminare dei caratteri che corrispondono a un determinato pattern. Questo può essere fatto per una serie di ragioni, ad esempio per la pulizia dei dati o per manipolare testo in modo specifico.

## Come Fare

Per eliminare dei caratteri che soddisfano un certo pattern, è possibile utilizzare la funzione `deleteBy` della libreria `Data.List`. Questa funzione accetta come parametri una funzione che confronta due elementi e una lista in cui cercare i corrispondenti da eliminare. Esempio di codice:

```Haskell
import Data.List

-- Definizione della funzione che controlla se un carattere è uguale a una vocale
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

-- Lista di caratteri di esempio
characters = "haskell"

-- Eliminazione di tutte le vocali dalla lista
output = deleteBy isVowel characters
```

L'output sarà: `"hskll"`, poiché tutte le vocali sono state eliminate dalla lista di caratteri.

## Approfondimento

La funzione `deleteBy` è un esempio di "higher-order function", ovvero una funzione che prende come parametro un'altra funzione. Questo è uno dei concetti fondamentali della programmazione funzionale, su cui si basa il linguaggio Haskell. Utilizzando funzioni come parametri, è possibile scrivere codice più generico e riutilizzabile.

Oltre alla funzione `deleteBy`, esistono altre funzioni nella libreria `Data.List` che permettono di eliminare elementi da una lista basandosi su patterns, come ad esempio `delete` o `filter`. Ogni funzione ha un suo specifico utilizzo e quindi è consigliato studiarle e capirne le differenze per poter scegliere quella più adatta al proprio caso d'uso.

## Vedi Anche

* [Documentazione sulla funzione `deleteBy` su Hoogle](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:deleteBy)
* [Ulteriori esempi e spiegazioni sull'utilizzo di funzioni di alta-ordine in Haskell](https://wiki.haskell.org/Higher_order_function)