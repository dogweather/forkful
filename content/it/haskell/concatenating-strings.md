---
title:    "Haskell: Concatenazione di stringhe"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché

Concatenare delle stringhe è un'operazione molto comune nella programmazione e può essere molto utile quando si vuole unire più stringhe per formare una singola stringa più lunga. Questo può essere utile, ad esempio, per la creazione di output in base a input variabili o per la costruzione di URL dinamiche.

## Come fare

Per concatenare delle stringhe in Haskell, si utilizza la funzione `++` oppure l'operatore `++`. Vediamo un esempio:

```Haskell
-- Definiamo due stringhe
let nome = "Alice"
let cognome = "Rossi"

-- Concateniamo le due stringhe utilizzando la funzione ++
nome ++ " " ++ cognome
```

Questo ci restituirà una singola stringa che unisce il nome e il cognome, in questo caso "Alice Rossi".

Inoltre, possiamo utilizzare l'operatore `++` all'interno di una funzione per concatenare più stringhe insieme. Ad esempio:

```Haskell
-- Definiamo una funzione che prende in input una lista di stringhe e le concatena insieme
concatena :: [String] -> String
concatena [] = "" -- Se la lista è vuota, restituiamo una stringa vuota
concatena (x:xs) = x ++ concatena xs -- Uniamo l'elemento attuale con il risultato della funzione passata al resto della lista

-- Utilizziamo la nostra funzione
concatena ["Ciao ", "a ", "tutti!"] -- Restituirà la stringa "Ciao a tutti!"
```

## Approfondimento

In Haskell, la concatenazione di stringhe è possibile grazie al fatto che le stringhe sono elenchi di caratteri. Ciò significa che quando si utilizza la funzione `++` o l'operatore `++`, si sta effettivamente concatenando due elenchi di caratteri. Inoltre, dato che le stringhe sono immutabili in Haskell, la concatenazione non modifica le stringhe di partenza ma ne crea una nuova che le unisce.

Un'altra cosa importante da notare è che la concatenazione è molto efficiente in Haskell grazie al concetto di "lazy evaluation". Ciò significa che quando si utilizza la concatenazione, Haskell non crea una nuova stringa ogni volta ma ne crea una soltanto quando viene effettivamente richiesta (ad esempio, quando viene stampata a schermo). Questo aiuta a evitare sprechi di memoria e rende la concatenazione molto efficiente.

## Vedi anche

- [Documentazione ufficiale di Haskell](https://www.haskell.org/)
- [10 funzioni utili in Haskell](https://www.robertodipisa.it/10-funzioni-utility-haskell/)
- [Haskell per principianti](https://www.haskell.org/)