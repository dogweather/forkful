---
title:                "Clojure: Estrazione di sottostringhe"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

# Perché estrae le sottostringhe?

Spesso, nei nostri programmi, ci troviamo ad avere a che fare con stringhe di testo molto lunghe. In questi casi, può essere utile dividere la stringa in sottostringhe più gestibili. Vediamo come fare questo utilizzando Clojure.

## Come fare

Per estrarre le sottostringhe in Clojure, possiamo utilizzare la funzione `subs`. Questa funzione ha due parametri: la stringa di partenza e gli indici di inizio e fine della sottostringa desiderata. Ad esempio:

```Clojure
(def test-str "Questo è un esempio di stringa")

(print (subs test-str 6 11))

;; output: è un
```

## Approfondimento

La funzione `subs` lavora in modo simile alla funzione `substring` di altri linguaggi di programmazione. Tuttavia, a differenza di quest'ultima, la funzione `subs` accetta anche numeri negativi come indici, che indicano una posizione partendo dalla fine della stringa.

Inoltre, possiamo anche utilizzare `subs` per estrarre una sottostringa a partire dalla fine della stringa, specificando un indice negativo come punto di partenza e uno positivo come punto di fine. Ad esempio:

```Clojure
(print (subs test-str -12 18))

;; output: mo di stringa
```

## Vedi anche

- Documentazione ufficiale di Clojure sulla funzione `subs`: https://clojuredocs.org/clojure.core/subs
- Tutorial su come manipolare le stringhe in Clojure: https://clojure.org/guides/strings
- Esempi di utilizzo della funzione `subs` in Clojure: https://rosettacode.org/wiki/Sub-string_extraction#Clojure