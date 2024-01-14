---
title:                "Clojure: Calcolo della lunghezza di una stringa"
simple_title:         "Calcolo della lunghezza di una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un programma che può trovare la lunghezza di una stringa può sembrare un compito banale, ma è in realtà un elemento cruciale della programmazione. Conoscere la lunghezza di una stringa può aiutare a manipolarla e utilizzarla in diversi modi. In questo articolo, esploreremo come trovare la lunghezza di una stringa in Clojure.

## Come fare

In Clojure, esistono diverse opzioni per trovare la lunghezza di una stringa. Possiamo utilizzare la funzione `count` o il metodo `length` in combinazione con la funzione `str` per calcolare la lunghezza della stringa. Ecco un esempio:

```Clojure
;; utilizzando la funzione count
(count "ciao") 
;; output: 4

;; utilizzando il metodo length
(str "ciao" length) 
;; output: 4
```

In entrambi i casi, abbiamo ottenuto un output di 4, poiché la stringa "ciao" è composta da 4 caratteri.

## Approfondimento

Oltre alle funzioni già menzionate, esistono altre opzioni per trovare la lunghezza di una stringa in Clojure che possono risultare utili in situazioni specifiche.

Utilizzando il metodo `split` possiamo suddividere una stringa in un vettore di sottostringhe e poi utilizzare la funzione `count` per ottenere la lunghezza di questo vettore. Ad esempio:

```Clojure
(count (.split "ciao mondo" " ")) 
;; output: 2

;; in questo caso, la stringa viene suddivisa in due elementi, "ciao" e "mondo".
```

Un'altra opzione è utilizzare la libreria `clojure.string` che contiene la funzione `length`, specifica per le stringhe. Con questa funzione possiamo contare direttamente i caratteri all'interno di una stringa, senza dover prima convertirla in un vettore di sottostringhe. Ad esempio:

```Clojure
;; utilizzando la funzione length della libreria clojure.string
(clojure.string/length "ciao") 
;; output: 4
```

Inoltre, è possibile utilizzare variabili all'interno della funzione `count` o `length` per trovare la lunghezza di una stringa dinamicamente. Ad esempio:

```Clojure
(def stringa "ciao")
(count stringa) 
;; output: 4
```

## Vedi anche

Per ulteriori informazioni sulla manipolazione di stringhe in Clojure, puoi consultare questi link:

- [Documentazione di Clojure sulle Stringhe](https://clojuredocs.org/clojure.core$defn$fn)
- [Articolo su Medium: "Stringhe e alberi in Clojure"](https://medium.com/@candorek/strings-and-trees-in-clojure-3f593e5222a0)