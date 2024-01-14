---
title:    "Clojure: Trovare la lunghezza di una stringa."
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Perché vorresti sapere la lunghezza di una stringa? Forse stai creando un programma per elaborare testi o hai bisogno di validare l'input dell'utente. In ogni caso, la conoscenza della lunghezza di una stringa è un'operazione fondamentale nella programmazione.

## Come fare

Per trovare la lunghezza di una stringa in Clojure, è possibile utilizzare la funzione `count` che conta il numero di elementi all'interno di una sequenza. Se si passa una stringa alla funzione `count`, restituirà il numero di caratteri nella stringa. Ecco un esempio di codice:

```Clojure
(def stringa "Ciao a tutti!")
(count stringa)
```

L'output sarà: `13`, poiché ci sono 13 caratteri nella stringa.

## Approfondimento

È importante ricordare che in Clojure la lunghezza di una stringa viene restituita come intero e non come una nuova stringa contenente il numero di caratteri. Inoltre, la funzione `count` può essere utilizzata su qualsiasi sequenza, non solo su stringhe.

Se si vuole ottenere il risultato come una stringa, è possibile utilizzare la funzione `str` per convertire il risultato in una stringa. Ad esempio:

```Clojure
(def stringa "Ciao a tutti!")
(str (count stringa))
```

L'output sarà: `"13"`.

## Vedi anche

- Documentazione ufficiale di Clojure sulla funzione `count`: https://clojuredocs.org/clojure.core/count
- Guida introduttiva alla programmazione in Clojure: https://cognitect.com/learn-clojure/