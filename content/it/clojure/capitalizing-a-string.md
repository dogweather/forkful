---
title:                "Maiuscolare una stringa"
html_title:           "Clojure: Maiuscolare una stringa"
simple_title:         "Maiuscolare una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Che cosa e perché?
Capitalizzare una stringa significa mettere la prima lettera in maiuscolo e il resto in minuscolo. I programmatori lo fanno per assicurarsi che il testo sia ben formattato e uniforme nelle loro applicazioni.

## Come fare:
```
Clojure
(-> "ciao mondo" .toUpperCase)
;; restituisce "Ciao mondo"
```

```
Clojure
(-> "HELLO WORLD" .toLowerCase)
;; restituisce "hello world"
```

## Approfondimento:
La capitalizzazione delle stringhe ha origini nel linguaggio di programmazione BASIC. In alternativa, i programmatori possono usare la funzione string/capitalize per ottenere lo stesso effetto in Clojure. L'implementazione della capitalizzazione di una stringa può variare a seconda del linguaggio di programmazione utilizzato.

## Vedi anche:
- [String/Capitalize - ClojureDocs](https://clojuredocs.org/clojure.core/string)
- [Capitalizzazione delle stringhe - Wikipedia](https://it.wikipedia.org/wiki/Capitalizzazione_delle_stringhe)