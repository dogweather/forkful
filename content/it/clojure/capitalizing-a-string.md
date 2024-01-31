---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
simple_title:         "Maiuscolizzare una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizzare una stringa significa convertire la prima lettera di ogni parola in maiuscolo, mantenendo le altre in minuscolo. I programmatori fanno questo per migliorare la leggibilità di titoli, nomi o per rispettare determinate convenzioni di formattazione.

## How to:
In Clojure, puoi capitalizzare una stringa usando la funzione `clojure.string/capitalize`. Per capitalizzare tutte le parole in una stringa, combina `clojure.string/split`, `map`, e `clojure.string/join`.

```Clojure
(require '[clojure.string :as str])

;; Capitalizza una singola parola
(str/capitalize "ciao")
;; => "Ciao"

;; Capitalizza ogni parola di una frase
(str/join " " (map str/capitalize (str/split "questo è un esempio" #" ")))
;; => "Questo È Un Esempio"
```

## Deep Dive
La capitalizzazione delle stringhe è una pratica comune in molti linguaggi di programmazione. In Clojure, la funzionalità standard tramite `clojure.string/capitalize` è limitata alla prima parola. Per superare questa limitazione, creiamo una funzione più generica.

Un'alternativa è usare le espressioni regolari per riconoscere i confini delle parole. Questa soluzione è più potente ma anche più complessa. Un'altra possibilità è usare librerie di terze parti che offrono maggiori funzionalità.

L'implementazione del capitolo "How to:" sfrutta le potenti funzioni di Clojure per le collezioni, come `map`, che applica una funzione a ogni elemento di una collezione, e `join`, che unisce elementi di una collezione in una stringa.

## See Also
- Documentazione ufficiale di Clojure per `clojure.string`: [https://clojure.github.io/clojure/clojure.string-api.html](https://clojure.github.io/clojure/clojure.string-api.html)
- Articolo sulla manipolazione delle stringhe in Clojure: [https://clojure.org/guides/weird_characters](https://clojure.org/guides/weird_characters)
