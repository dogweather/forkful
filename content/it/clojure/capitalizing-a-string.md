---
title:                "Capitalizzare una stringa"
html_title:           "Clojure: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Hai mai avuto la necessità di convertire la prima lettera di una stringa in maiuscolo? Forse stai lavorando su un progetto che richiede un output in un formato specifico o forse vuoi semplicemente migliorare l'estetica della tua applicazione. In entrambi i casi, ci sono diverse funzioni in Clojure che ti permettono di capitalizzare una stringa in modo semplice e veloce.

## Come fare

```Clojure
;; usando la funzione "capitalize"
(capitalize "clojure") ; output: "Clojure"

;; usando la funzione "capitalize-first"
(capitalize-first "clojure") ; output: "Clojure"

;; usando la funzione "clojure.string/capitalize"
(require '[clojure.string :as str])
(str/capitalize "clojure") ; output: "Clojure"

;; usando la funzione "clojure.string/capitalize-first"
(str/capitalize-first "clojure") ; output: "Clojure"
```

## Approfondimento

Le funzioni "capitalize" e "capitalize-first" si comportano in maniera simile: convertono la prima lettera di una stringa in maiuscolo e lasciano inalterate le altre. La differenza è che "capitalize" converte in maiuscolo anche tutte le altre lettere della stringa, mentre "capitalize-first" le mantiene in minuscolo.

La funzione "clojure.string/capitalize" fa parte della libreria standard di Clojure e può essere utilizzata senza dover importare il modulo. Invece, "clojure.string/capitalize-first" richiede l'importazione della libreria "clojure.string".

Inoltre, è importante notare che queste funzioni non modificano la stringa originale, ma restituiscono una nuova stringa capitalizzata.

## Vedi anche

- Documentazione ufficiale di Clojure sulle funzioni "capitalize" e "capitalize-first": https://clojuredocs.org/clojure.core/capitalize
- Documentazione ufficiale di Clojure sulla libreria "clojure.string": https://clojuredocs.org/clojure.string
- Tutorial su come capitalizzare una stringa in Clojure: https://www.tutorialspoint.com/clojure/clojure_strings.htm