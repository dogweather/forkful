---
title:                "Haskell: Unire le stringhe"
simple_title:         "Unire le stringhe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

L'unione (o concatenazione) di stringhe è un'operazione molto comune nella programmazione, soprattutto in linguaggi funzionali come Haskell. Questo ci permette di combinare diverse stringhe e creare un'unica stringa più lunga.

## Come Fare

Per concatenare stringhe in Haskell, possiamo utilizzare l'operatore `++` o la funzione `concat`. Vediamo un esempio di entrambi:

````Haskell
"Hello " ++ "world"
-- Output: "Hello world"

concat ["H", "e", "l", "l", "o"]
-- Output: "Hello"
````

In questo esempio, abbiamo utilizzato l'operatore `++` per unire le due stringhe "Hello" e "world", ottenendo "Hello world". Con la funzione `concat`, invece, abbiamo dato in input una lista di stringhe e ottenuto in output la loro concatenazione.

Possiamo anche concatenare più di due stringhe utilizzando `++` e `concat` in sequenza, a seconda delle nostre esigenze. Inoltre, è possibile concatenare stringhe con altri tipi di dati, purché siano convertibili in stringhe.

## Approfondimento

In Haskell, l'operatore `++` e la funzione `concat` sono molto efficienti, in quanto lavorano in modo "ricorsivo" al contrario. Ciò significa che anziché unire le stringhe dalla prima alla ultima, uniscono le ultime stringhe prima e procedono a ritroso fino alla prima.

Ad esempio, per concatenare le stringhe "Hello" e "world", `++` agirà come segue:

- "Hello " ++ "world"
- "Hello" ++ " world"
- "Hello" ++ "w" ++ "ord"
- "H" ++ "ello" ++ "w" ++ "ord"
- "H" ++ "e" ++ "llo" ++ "w" ++ "ord"
- "H" ++ "e" ++ "l" ++ "lo" ++ "w" ++ "ord"
- "H" ++ "e" ++ "l" ++ "l" ++ "o" ++ "w" ++ "ord"
- "H" ++ "e" ++ "l" ++ "l" ++ "ow" ++ "ord"
- "H" ++ "e" ++ "l" ++ "lo" ++ "wor" ++ "d"
- "H" ++ "e" ++ "ll" ++ "ow" ++ "ord"
- "H" ++ "ell" ++ "o" ++ "world"
- "Hell" ++ "oworld"
- "Hello" ++ "world"
- "Hello world"

Inoltre, le operazioni di concatenazione in Haskell sono associative, quindi l'ordine delle stringhe non interferisce con il risultato finale.

## Vedi Anche

- [Haskell: Lista di Stringhe](https://www.tutorialspoint.com/haskell/haskell_lists.htm)
- [Haskell: Concatenate Stringhe](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Haskell Funzioni Ricorsive](https://www.tutorialspoint.com/haskell/haskell_recursive_functions.htm)