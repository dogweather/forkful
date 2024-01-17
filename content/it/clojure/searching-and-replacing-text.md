---
title:                "Ricerca e sostituzione di testo"
html_title:           "Clojure: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
La ricerca e la sostituzione di testo è un'operazione comune nella programmazione. Consiste nel trovare un determinato testo all'interno di una stringa e sostituirlo con un altro. I programmatori lo fanno per semplificare il processo di modifica del codice e per automatizzare alcune operazioni ripetitive.

## Come:
Un modo per cercare e sostituire il testo in Clojure è utilizzare la funzione `replace`. Qui di seguito un esempio di codice che cerca tutte le occorrenze della parola "ciao" in una stringa e le sostituisce con "hello":

```Clojure
(def s "ciao mondo ciao")
(replace s "ciao" "hello")
```

Questo produrrà l'output: "hello mondo hello". Si può anche utilizzare la funzione `replace-first` se si vuole sostituire solo la prima occorrenza.

## Approfondimento:
La ricerca e la sostituzione di testo è stata introdotta con il linguaggio di programmazione awk negli anni '70, ma è da allora diventata una funzione comune in molti linguaggi di programmazione. In Clojure, ci sono anche altre funzioni utili per questo scopo, come `replace-re` per utilizzare espressioni regolari nella ricerca e sostituzione di testo. Inoltre, ci sono anche librerie di terze parti, come clojure.string, che offrono funzionalità più avanzate per la manipolazione di stringhe.

## Vedi anche:
- La documentazione ufficiale di Clojure su `replace`: https://clojuredocs.org/clojure.core/replace
- Libreria clojure.string: https://clojure.github.io/clojure/clojure.string-api.html