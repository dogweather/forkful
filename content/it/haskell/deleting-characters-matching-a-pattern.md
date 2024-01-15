---
title:                "Eliminazione di caratteri corrispondenti a uno schema"
html_title:           "Haskell: Eliminazione di caratteri corrispondenti a uno schema"
simple_title:         "Eliminazione di caratteri corrispondenti a uno schema"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché
Hai mai avuto la necessità di eliminare dei caratteri all'interno di una stringa che corrispondono ad un certo pattern? In questo articolo ti mostrerò come fare questo in modo semplice utilizando Haskell.

## Come Fare
Per iniziare, dovrai assicurarti di avere installato Haskell sul tuo computer. Una volta fatto ciò, puoi aprire il terminale e creare un nuovo file Haskell con il comando `touch caratteri.hs`.

Ora puoi aprire il file appena creato con il tuo editor di testo preferito e iniziare a scrivere il codice. Prima di tutto, dovrai importare il modulo `Data.List` che ci permetterà di utilizzare alcune funzioni utili per l'eliminazione di caratteri all'interno di una stringa. Puoi farlo inserendo questa riga di codice all'inizio del tuo file:

```
import Data.List
```

Per eliminare i caratteri corrispondenti ad un pattern in una stringa, utilizzeremo la funzione `filter` che accetta come parametri una funzione che testa ogni carattere della stringa e la stringa stessa. Ad esempio, se volessimo eliminare tutti i numeri da una stringa possiamo utilizzare questa funzione:

```
eliminaNumeri :: [Char] -> [Char]
eliminaNumeri s = filter (`notElem` "0123456789") s
```

Per testare questa funzione, puoi chiamarla inserendo una stringa come parametro all'interno del tuo file:

```
main = print (eliminaNumeri "Questa è una stringa con 123 numeri.")
```

Se esegui il tuo programma con il comando `runhaskell caratteri.hs`, dovresti vedere l'output seguente:

```
"Questa una stringa con numeri."
```

Hai eliminato con successo tutti i numeri dalla stringa utilizzando la funzione `eliminaNumeri` che abbiamo definito in precedenza.

## Deep Dive
La funzione `filter` che abbiamo utilizzato è una funzione molto comune in Haskell e viene utilizzata per filtrare una lista di elementi restituendo solo quelli che soddisfano una certa condizione. In questo caso, la nostra funzione `eliminaNumeri` utilizza la funzione `notElem` che verifica se un elemento non appartiene ad una lista. Questo ci permette di specificare un insieme di caratteri che vogliamo eliminare dalla stringa.

Nella nostra implementazione, abbiamo utilizzato le stringhe  "0123456789" e " " (uno spazio) come parametri per la funzione `notElem`. Tuttavia, possiamo utilizzare qualsiasi stringa per eliminare i caratteri corrispondenti ad un pattern specifico. Ad esempio, potremmo voler eliminare tutti i caratteri maiuscoli da una stringa, possiamo farlo utilizzando questa funzione:

```
eliminaMaiuscole :: [Char] -> [Char]
eliminaMaiuscole s = filter (`notElem` "QWERTYUIOPASDFGHJKLZXCVBNM") s
```

Questa funzione eliminerebbe tutti i caratteri maiuscoli dalla stringa che gli viene passata come parametro. Ci sono molte altre funzioni utili che possiamo utilizzare con `filter`, come ad esempio `elem` che viene utilizzata per verificare se un elemento appartiene ad una lista.

## Vedi Anche
- [Haskell Wiki](https://wiki.haskell.org/Haskell)
- [Haskell.org](https://www.haskell.org/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)