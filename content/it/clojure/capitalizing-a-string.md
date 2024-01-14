---
title:    "Clojure: Maiuscolo di una stringa"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitando una stringa può essere utile quando si desidera una visualizzazione più pulita dei dati o quando si vuole enfatizzare una particolare parola o frase. Inoltre, può essere necessario per adattarsi a determinati standard di formattazione.

## Come fare

Per capitalizzare una stringa in Clojure, è possibile utilizzare la funzione ```capitalize``` del core library. Questa funzione prende come input una stringa e restituisce la stessa stringa con la prima lettera convertita in maiuscolo. Ad esempio:

```Clojure
(capitalize "ciao a tutti")
;; output: "Ciao a tutti"
```

Se si vogliono capitalizzare tutte le parole di una stringa, si può utilizzare la funzione ```clojure.string/capitalize-words``` del namespace ```clojure.string```. Questa funzione prende in input una stringa e restituisce la stessa stringa con le prime lettere di ogni parola convertite in maiuscolo. Ad esempio:

```Clojure
(clojure.string/capitalize-words "ciao a tutti")
;; output: "Ciao A Tutti"
```

Si può anche utilizzare la funzione ```clojure.string/upper-case``` per convertire tutte le lettere di una stringa in maiuscolo. Ad esempio:

```Clojure
(clojure.string/upper-case "ciao a tutti")
;; output: "CIAO A TUTTI"
```

## Approfondimento

La funzione ```capitalize``` utilizza la funzione ```character.toLowerCase``` del Java SDK per convertire la prima lettera in minuscolo. Ciò significa che la stringa deve essere inizializzata correttamente con le librerie Java per il funzionamento della funzione. Inoltre, la funzione ```capitalize-words``` utilizza la funzione ```capitalize``` per ogni parola nella stringa, quindi funziona solo con parole separate da spazi. 

## Vedi anche

- Documentazione ufficiale di Clojure su ```capitalize```: https://clojuredocs.org/clojure.core/capitalize
- Documentazione ufficiale di Clojure sul namespace ```clojure.string```: https://clojuredocs.org/clojure.string
- Documentazione ufficiale di Java su ```character.toLowerCase```: https://docs.oracle.com/javase/7/docs/api/java/lang/Character.html#toLowerCase(char)