---
title:                "Clojure: Capitalizzazione di una stringa"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molti sviluppatori si trovano spesso a dover manipolare le stringhe all'interno dei loro programmi. Talvolta è necessario trasformare una stringa in maiuscolo per questioni di formattazione o confronto con altre stringhe. In questo post vedremo come capitalizzare una stringa in Clojure e perché potrebbe essere utile farlo.

## Come Fare

Per capitalizzare una stringa in Clojure, possiamo utilizzare la funzione `capitalize`. Prende in input una stringa e restituisce la stessa stringa con la prima lettera maiuscola.

```Clojure
(capitalize "ciao") ;; Output: "Ciao"
(capitalize "ciao, come va?") ;; Output: "Ciao, come va?"
```

Se vogliamo capitalizzare tutte le parole all'interno di una stringa, possiamo usare la funzione `clojure.string/capitalize-words`.

```Clojure
(require '[clojure.string :as str])
(str/capitalize-words "ciao, come va?") ;; Output: "Ciao, Come Va?"
```

Nonostante sia una funzione semplice, `capitalize` potrebbe risolvere problemi di formattazione o di confronto tra stringhe all'interno del nostro codice.

## Approfondimento

La funzione `capitalize` in realtà utilizza il protocollo `clojure.string/Capitalizable`. Questo protocollo viene implementato automaticamente da ogni tipo di dato che ha un'implementazione di `clojure.lang.Capitalizable`.

Questo significa che possiamo utilizzare `capitalize` anche su tipi di dato che non sono stringhe, come ad esempio numeri.

```Clojure
(capitalize 123) ;; Output: 123
```

Inoltre, il protocollo `clojure.string/Join` è stato implementato anche per essere utilizzato con `capitalize`. Questo ci permette di unire più stringhe e capitalizzarle tutte insieme.

```Clojure
(require '[clojure.string :as str])
(str/join " " (capitalize "ciao" "come" "va?")) ;; Output: "Ciao Come Va?"
```

In generale, le stringhe sono delle strutture molto versatile e flessibile in Clojure e grazie a funzioni come `capitalize` possiamo manipolarle in modi molto interessanti.

## Vedi Anche

- Documentazione ufficiale di Clojure sulla funzione `capitalize`
https://clojuredocs.org/clojure.string/capitalize

- Overview dei protocolli in Clojure
https://clojure.org/reference/protocols