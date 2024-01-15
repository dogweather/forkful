---
title:                "Unire le stringhe."
html_title:           "Clojure: Unire le stringhe."
simple_title:         "Unire le stringhe."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Se stai programmando in Clojure, potresti trovarsi nella situazione di dover unire una serie di stringhe in una sola stringa più lunga. Ciò può essere utile per creare un output formattato o per gestire stringhe dinamiche all'interno di una funzione.

## Come fare

Per unire una serie di stringhe in Clojure, è possibile utilizzare la funzione `str`, che accetta come argomenti una o più stringhe e restituisce una nuova stringa contenente gli argomenti concatenati. Ad esempio:

```Clojure
(str "Questo" " è un" "esempio")
```

Questo codice producirà l'output `Questo è un esempio`.

## Approfondimenti

La funzione `str` accetta anche argomenti non stringa, in questo caso verranno convertiti in stringhe prima di essere concatenati. Inoltre, è possibile utilizzare la funzione `str/join` per unire una sequenza di stringhe con uno specifico separatore. Ad esempio:

```Clojure
(str/join "-" ["Hello" "world"])
```

Questo codice restituirà `Hello-world`.

## Vedi anche

- Documentazione ufficiale sulla funzione `str` in Clojure: https://clojuredocs.org/clojure.core/str
- Altri metodi per manipolare le stringhe in Clojure: https://clojuredocs.org/clojure.string