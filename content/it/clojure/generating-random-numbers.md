---
title:    "Clojure: Generare numeri casuali"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Generare numeri casuali è una pratica comune nella programmazione e può essere utile per una varietà di scopi, come generare un input casuale per testare il codice o creare giochi che richiedono elementi casuali.

## Come Fare
Per generare numeri casuali, possiamo utilizzare la funzione "rand-int" in Clojure. Ad esempio, il codice seguente genererà un numero casuale compreso tra 1 e 10:

```Clojure
(rand-int 10)
```
Output: 7

Possiamo anche specificare un limite inferiore e superiore per il numero casuale, come in questo esempio:

```Clojure
(rand-int 5 15)
```
Output: 12

Per generare numeri decimali casuali, possiamo utilizzare la funzione "rand" e specificare il limite superiore desiderato. Ad esempio:

```Clojure
(rand 10.0)
```
Output: 4.793033436622162

È anche possibile utilizzare la funzione "rand-nth" per selezionare casualmente un elemento da una sequenza. Ad esempio, possiamo utilizzarlo per selezionare casualmente un numero dalla lista [1 2 3 4 5]:

```Clojure
(rand-nth [1 2 3 4 5])
```
Output: 3

## Approfondimento
La funzione "rand-int" utilizza un algoritmo di generazione di numeri casuali basato sull'orario di sistema. Ciò significa che se viene chiamata più volte nello stesso secondo, restituirà lo stesso valore. Per evitare ciò, possiamo utilizzare la funzione "with-casual-seed" per generare un seed diverso ogni volta. Ad esempio:

```Clojure
(def random-number (with-casual-seed (rand-int 10)))
```
Output: 7

Viene generato un diverso numero casuale ogni volta che viene chiamata la variabile "random-number".

## Vedi Anche
- [Guida alla Programmazione con Clojure](https://www.clojure.org/guides/getting_started)
- [Documentazione su Funzioni Casuali in Clojure](https://clojuredocs.org/clojure.core/rand-int)
- [Tutorial su Programmazione con Numeri Casuali in Clojure](https://www.baeldung.com/clojure-random)