---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-01-19
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Le espressioni regolari (regex) sono modelli per cercare e manipolare testo. Sono usate dai programmatori per validare, estrarre o sostituire parti di stringhe con precisione e velocità.

## How to (Come fare):
Ecco alcuni esempi in Clojure:

```Clojure
;; Trovare una corrispondenza
(re-find #"\d+" "L'anno è 2023") ; => "2023"

;; Trovare tutte le corrispondenze
(re-seq #"\d+" "Appuntamento alle 9 e poi alle 17") ; => ("9" "17")

;; Sostituire una corrispondenza
(clojure.string/replace "3 gatti, 2 cani" #"\d" "*") ; => "* gatti, * cani"

;; Validare una stringa rispetto a un pattern
(boolean (re-matches #"\d{4}" "2023")) ; => true
(boolean (re-matches #"\d{4}" "anno")) ; => false
```

## Deep Dive (Analisi approfondita)

1. Storia: Le espressioni regolari sono nate negli anni '50.
2. Alternative: Altre opzioni includono il parsing manuale o l'utilizzo di librerie per l'analisi sintattica (parsing).
3. Dettagli di implementazione: Regex in Clojure è basato su Java, quindi segue il set di funzionalità Java Pattern class.

## See Also (Vedi anche)

- Clojure Documentation: https://clojure.org/guides/learn/functions#_regex
- Java Pattern class: https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html
- Online Regex Tester (for experimentation): https://regexr.com/
