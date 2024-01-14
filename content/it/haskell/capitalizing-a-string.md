---
title:                "Haskell: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché Capitalizzare una Stringa è Importante

Capitalize una stringa è un'operazione comune nella programmazione quando si vuole rendere la prima lettera maiuscola, ad esempio in un titolo o in un nome. Potrebbe sembrare una banalità, ma può fare la differenza nella presentazione dei dati e migliorare l'estetica del codice.

## Come Capitalizzare una Stringa in Haskell

Per capitalizzare una stringa in Haskell, possiamo utilizzare la funzione `toUpper` della libreria `Data.Char`. Vediamo un esempio pratico:

```Haskell
import Data.Char (toUpper)

main :: IO ()
main = do
  let str = "ciao a tutti"
  putStrLn (toUpper (head str) : tail str)
```
In questo esempio, utilizziamo la funzione `toUpper` per convertire la prima lettera della stringa `str` in maiuscolo e poi la concateniamo con il resto della stringa utilizzando l'operatore `:`. Il risultato dell'esecuzione del codice sarà il seguente:

```
"Ciao a tutti"
```

## Approfondimento sulla Capitalizzazione in Haskell

La funzione `toUpper` è solo un semplice esempio di come si può capitalizzare una stringa in Haskell. Tuttavia, esistono altre funzioni e metodi che possono essere utilizzati a seconda del tipo di dati che stiamo manipolando e del risultato che vogliamo ottenere.

Ad esempio, se vogliamo capitalizzare tutte le lettere di una stringa, possiamo utilizzare la funzione `map` insieme alla funzione `toUpper` che verrà applicata ad ogni carattere della stringa. Ecco un esempio:

```Haskell
import Data.Char (toUpper)

main :: IO ()
main = do
  let str = "ciao a tutti"
  putStrLn (map toUpper str)
```
Il risultato dell'esecuzione di questo codice sarà:

```
"CIAO A TUTTI"
```

Inoltre, esistono anche funzioni che permettono di capitalizzare solo alcune lettere della stringa, come ad esempio la funzione `toTitle` che converte in maiuscolo solo le lettere che seguono uno spazio. È importante esplorare le diverse opzioni disponibili per scegliere la migliore soluzione in base alle nostre esigenze.

## Vedi Anche

- [Haskell Data.Char library](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Haskell map function](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:map)
- [Haskell string functions](https://www.haskell.org/documentation/#string-functions)