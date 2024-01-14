---
title:    "Haskell: Convertire una stringa in minuscolo"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché

La conversione di una stringa in lettere minuscole è un'operazione comune nella programmazione, utile per confrontare stringhe senza tenere conto di maiuscole o minuscole, o per uniformare l'input dell'utente. In Haskell, esistono diversi metodi per ottenere questo risultato.

## Come Fare

Per convertire una stringa in lettere minuscole, si può utilizzare la funzione `map` per applicare la funzione `toLower` a ciascun carattere della stringa. Ad esempio:

```Haskell
import Data.Char (toLower)

convertiMinuscolo :: String -> String
convertiMinuscolo str = map toLower str
```

La funzione `convertiMinuscolo` prende in input una stringa e restituisce una nuova stringa con tutti i caratteri in minuscolo. Possiamo testare questa funzione utilizzando il seguente codice:

```Haskell
convertiMinuscolo "CIAO A TUTTI!" -- Output: "ciao a tutti!"
convertiMinuscolo "HELLO world" -- Output: "hello world"
```

Se si desidera convertire solo la prima lettera di una stringa in minuscolo, si può utilizzare la funzione `uncapitalize` dal pacchetto `text`. Ad esempio:

```Haskell
import Data.Text (uncapitalize)

convertiPrimaLetteraMinuscolo :: String -> String
convertiPrimaLetteraMinuscolo str = uncapitalize str
```

La funzione `convertiPrimaLetteraMinuscolo` prende in input una stringa e restituisce la stessa stringa con la prima lettera in minuscolo. Vediamo un esempio di utilizzo:

```Haskell
convertiPrimaLetteraMinuscolo "CIAO A TUTTI!" -- Output: "cIAO A TUTTI!"
convertiPrimaLetteraMinuscolo "HELLO world" -- Output: "hELLO world"
```

## Approfondimento

In Haskell, le stringhe sono rappresentate come liste di caratteri (type synonym `String = [Char]`). Per questo motivo, possiamo utilizzare le funzioni che operano sulle liste per manipolare le stringhe.

Ad esempio, possiamo utilizzare la funzione `foldr` per convertire una stringa in lettere minuscole:

```Haskell
convertiMinuscoloConFoldr :: String -> String
convertiMinuscoloConFoldr = foldr (\char acc -> toLower char : acc) ""
```

La funzione `convertiMinuscoloConFoldr` prende in input una stringa e restituisce una nuova stringa in cui ogni carattere è stato convertito in minuscolo. La funzione `foldr` applica la lambda function a ogni carattere della stringa partendo dalla fine. Vediamo un esempio di utilizzo:

```Haskell
convertiMinuscoloConFoldr "CIAO A TUTTI!" -- Output: "ciao a tutti!"
convertiMinuscoloConFoldr "HELLO world" -- Output: "hello world"
```

## Vedi Anche

- [Hackage - Data.Char](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Hackage - Data.Text](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)