---
title:                "Haskell: Trova la lunghezza di una stringa"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

A volte, quando si scrive un codice, è necessario sapere quanti caratteri ci sono in una stringa. Questo può essere utile per validare l'input dell'utente, per fare operazioni di manipolazione di testo o semplicemente per ottenere informazioni su una variabile di tipo stringa. In questo post, impareremo come trovare la lunghezza di una stringa in Haskell.

## Come fare

Per calcolare la lunghezza di una stringa in Haskell, possiamo utilizzare la funzione integrata `length`. Questa funzione accetta una stringa come argomento e restituisce un intero che rappresenta il numero di caratteri nella stringa.

```Haskell
length "Ciao!"     -- Output: 5
length "Haskell"   -- Output: 7
```

Possiamo anche applicare questa funzione ad una variabile di tipo stringa.

```Haskell
let myString = "Buongiorno"
length myString    -- Output: 10
```

Una delle caratteristiche interessanti di Haskell è che possiamo applicare funzioni agli argomenti usando il concetto di "composition". Ciò significa che possiamo combinare due o più funzioni per ottenere un risultato più complesso. Ad esempio, possiamo applicare la funzione `length` ad una stringa convertita in maiuscolo usando la funzione `map`.

```Haskell
map toUpper "ciao"        -- Output: "CIAO"
length (map toUpper "ciao") -- Output: 4
```

Un'altra funzione utile per la manipolazione di stringhe è `take`, che accetta un numero intero e una stringa e restituisce una sottostringa di lunghezza pari al numero specificato. Possiamo combinare queste due funzioni per ottenere la prima metà di una stringa.

```Haskell
take (length myString `div` 2) myString    -- Output: "Buon"
```

## Approfondimento

Ora che sappiamo come trovare la lunghezza di una stringa in Haskell, possiamo esplorare come funziona la funzione `length` in dettaglio. In realtà, `length` è definita come una funzione ricorsiva nel modulo `Prelude` di Haskell.

```Haskell
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs
```

Questa definizione può sembrare un po' strana all'inizio, ma è in realtà molto semplice. La funzione `length` accetta una lista di elementi di qualsiasi tipo (`a`) e restituisce un intero (`Int`). La prima riga della definizione è chiamata "caso base" e gestisce il caso in cui la lista è vuota, restituendo semplicemente 0. La seconda riga, invece, sfida la lista con il pattern matching, prendendo il primo elemento (`x`) e il resto della lista (`xs`). Quindi somma 1 al risultato della chiamata ricorsiva della funzione `length` sul resto della lista.

In sostanza, la funzione `length` scorre ricorsivamente la lista finché non raggiunge il caso base, contando il numero di elementi nella lista man mano che procede.

## Vedi anche

- Come usare le funzioni `map` e `length` in Haskell: https://www.tutorialspoint.com/How-can-I-use-map-and-length-functions-in-Haskell
- Una guida completa alle funzioni per la manipolazione di stringhe in Haskell: https://wiki.haskell.org/wikiupload/9/97/TMR-Issue13.pdf
- Il modulo `Prelude` di Haskell: https://downloads.haskell.org/~ghc/8.4.3/docs/html/libraries/base-4.11.1.0/Prelude.html