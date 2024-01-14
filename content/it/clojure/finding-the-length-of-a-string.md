---
title:                "Clojure: Trovare la lunghezza di una stringa"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Se stai imparando Clojure, è importante capire come trovare la lunghezza di una stringa. Questa è una delle operazioni più comuni quando si lavora con dati di testo e può essere utile in una varietà di situazioni.

## Come fare

Per trovare la lunghezza di una stringa in Clojure, puoi utilizzare la funzione `count`. Per esempio, supponiamo di avere la seguente stringa:

```Clojure
(def stringa "Ciao Mondo!")
```

Per trovare la sua lunghezza, possiamo scrivere il seguente codice:

```Clojure
(count stringa)
```

Questo dovrebbe restituire il valore 12, poiché ci sono 12 caratteri nella stringa "Ciao Mondo!".

## Approfondimento

Se desideri una soluzione più flessibile per trovare la lunghezza di una stringa, puoi anche utilizzare la funzione `seq` in combinazione con la funzione `count`. La funzione `seq` converte una stringa in una sequenza di caratteri, che può essere successivamente contata utilizzando la funzione `count`.

Ad esempio:

```Clojure
(def stringa "Ciao Mondo!")
(count (seq stringa))
```

Questo dovrebbe ancora restituire il valore 12.

Un altro aspetto importante da considerare è che la funzione `count` può anche essere utilizzata per trovare la dimensione di altre strutture dati in Clojure, come ad esempio vettori e mappe. Ad esempio, supponiamo di avere il seguente vettore:

```Clojure
(def vettore ["Uno" "Due" "Tre" "Quattro"])
```

Per trovare la sua dimensione, possiamo scrivere il seguente codice:

```Clojure
(count vettore)
```

Questo dovrebbe restituire il valore 4, poiché ci sono 4 elementi nel vettore.

## Vedi anche

- [Documentazione ufficiale Clojure sulla funzione `count`](https://clojuredocs.org/clojure.core/count)
- [Esempi di utilizzo della funzione `count` in Clojure](https://lispcast.com/clojure-count/)
- [Tutorial su come contare il numero di caratteri in una stringa in Clojure](https://blog.klipse.tech/clojure/2016/08/05/count-characters-clojure.html)