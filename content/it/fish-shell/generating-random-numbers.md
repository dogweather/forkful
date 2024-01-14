---
title:    "Fish Shell: Generazione di numeri casuali"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché
Generare numeri casuali è una funzione essenziale in molti programmi. Può essere utilizzato per creare scenari di gioco diversificati, generare password sicure e testare l'efficacia di algoritmi.

## Come
Per generare numeri casuali utilizzando Fish Shell, possiamo utilizzare il comando `math` seguito dalla funzione `random` e il numero massimo che si desidera generare. Ad esempio, per generare un numero casuale tra 1 e 10, possiamo scrivere:
```Fish Shell
math random 10
```
Questo ci darà un output come:
```
7
```
Possiamo anche utilizzare la funzione `seq` per generare una sequenza di numeri casuali. Ad esempio, per generare una sequenza di 5 numeri casuali tra 1 e 100, possiamo usare:
```Fish Shell
seq 5 | math random 100
```
Ciò produrrà un output come:
```
23
57
82
13
44
```
Inoltre, il comando `shuf` può essere utilizzato per generare una sequenza di numeri casuali senza dover specificare il numero massimo. Basta passare la lunghezza della sequenza come parametro:
```Fish Shell
shuf -r -e 10
```
Questo ci darà un output come:
```
5
8
1
10
3
```

## Deep Dive
Fish Shell utilizza la libreria `libmath` del C per generare numeri casuali. Ciò significa che i numeri generati da `math random` sono basati sull'algoritmo di generazione di numeri casuali del C. Se vogliamo avere più controllo sulla generazione dei numeri, possiamo utilizzare la funzione `open`. Ad esempio, per generare numeri casuali con una distribuzione normale utilizzando la libreria `librandom`:
```Fish Shell
open /dev/random or /dev/urandom
```
Inoltre, possiamo specificare un seed per la generazione dei numeri utilizzando la funzione `srandom`. Questo ci permette di ottenere una sequenza di numeri casuali riproducibile se utilizziamo lo stesso seed.

## Vedi anche
- [Documentazione Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guida alla programmazione in Fish Shell](https://fishshell.com/docs/current/commands.html#math)
- [Manuale di riferimento di libmath](https://man7.org/linux/man-pages/man7/libmath.7.html)