---
title:                "Avviare un nuovo progetto"
html_title:           "Haskell: Avviare un nuovo progetto"
simple_title:         "Avviare un nuovo progetto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Perché

Haskell è un linguaggio di programmazione funzionale pura che offre un approccio unico alla scrittura di codice. Se sei stanco dei linguaggi di programmazione convenzionali e vuoi esplorare nuove idee, allora Haskell potrebbe essere il linguaggio perfetto per il tuo prossimo progetto.

## Come fare

Per iniziare con Haskell, è necessario prima scaricare il compilatore Haskell, chiamato "GHC (Glasgow Haskell Compiler)", dal sito ufficiale. Una volta installato, puoi creare un nuovo progetto Haskell con il seguente comando:

```
haskell-new progetto-nuovo
```

Questo creerà una nuova directory chiamata "progetto-nuovo" con una struttura di progetto predefinita. Entriamo nella directory e apriamo il file "Main.hs", che è il file principale del nostro progetto.

La sintassi di base di Haskell è molto semplice e flessibile. Per esempio, per dichiarare una variabile "x" con il valore 10, basta scrivere:

```
let x = 10
```

Per scrivere una funzione che calcoli l'area di un cerchio dato il suo raggio:

```
areaCerchio raggio = pi * raggio^2
```

Possiamo chiamare questa funzione con un valore specifico come:

```
areaCerchio 5
```

che restituirebbe l'area di un cerchio con raggio di 5 unità.

## Approfondimento

Una cosa fantastica di Haskell è che possiamo definire funzioni più astratte utilizzando la ricorsione e funzioni di ordine superiore. Ad esempio, possiamo scrivere una funzione di somma che accetta una lista di numeri usando la ricorsione:

```
somma [] = 0
somma (x:xs) = x + somma xs
```

Questa funzione accetta una lista vuota e restituisce 0, o prende il primo elemento della lista ("x") e lo aggiunge alla somma del resto della lista ("xs").

Inoltre, possiamo anche scrivere funzioni di ordine superiore, cioè funzioni che accettano altre funzioni come argomenti. Ad esempio:

```
applicaDue f x = f (f x)
```

Questa funzione accetta una funzione ("f") e un valore ("x") e applica due volte la funzione al valore fornito. Possiamo utilizzare questa funzione per applicare una funzione di somma a un valore specifico due volte:

```
applicaDue (somma [1,2]) 5
```

che restituirà 8 come risultato.

## Vedi anche

- [GHC Download](https://www.haskell.org/ghc/download.html)
- [Haskell Wiki](https://wiki.haskell.org/)