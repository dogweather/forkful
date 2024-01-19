---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Eliminare i caratteri che corrispondono a un modello riguarda la rimozione di stringhe specifiche da un testo. I programmatori lo fanno per rendere il loro codice più pulito, più efficiente e più facile da leggere.

## Come fare:
Per proporre un esempio di codice che elimina i caratteri corrispondenti a un modello in Clojure, useremo le funzioni `re-seq` e `join`. Supponiamo che vogliamo rimuovere tutti i numeri da una stringa.

```Clojure
(defn remove-numbers [s]
  (clojure.string/join (re-seq #"[^\d]" s)))

(println (remove-numbers "c1od2ic3e4")) 
;; Output: "codice"
```

Questo codice rimuove tutti i numeri dalla stringa "c1od2ic3e4" e stampa "codice".

## Approfondimento
Similmente ad altre varianti di Lisp, Clojure fornisce una serie di operatori per manipolare le stringhe. L'approccio del modello regolare è comune in molti linguaggi di programmazione. Puoi anche utilizzare `filter` e `not` in combinazione con `Character/isDigit`.

La scelta del metodo è principalmente una questione di gusto e di contesto. L'importante è mantenere il codice comprensibile e mantenibile.

```Clojure
(defn remove-numbers [s]
  (let [not-digit? #(not (Character/isDigit %))]
    (apply str (filter not-digit? s))))
   
(println (remove-numbers "c1od2ic3e4"))
;; Output: "codice"
```

## Vedi Anche
Puoi ampliare le tue conoscenze sulla manipolazione delle stringhe in Clojure consultando le seguenti risorse:

1. Clojure - Stringhe: https://clojuredocs.org/clojure.string
2. Le espressioni regolari in Clojure: https://clojuredocs.org/clojure.core/re-seq
3. Filtro in Clojure: https://clojuredocs.org/clojure.core/filter