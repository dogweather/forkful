---
title:                "Concatenazione di stringhe"
date:                  2024-01-20T17:34:52.497455-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenazione di stringhe"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenare le stringhe significa unire due o più sequenze di caratteri. I programmatori lo fanno per costruire messaggi, combinare dati o semplicemente per generare una nuova stringa a partire da pezzi esistenti.

## Come Fare:
La funzione `str` in Clojure combina stringhe. Ecco un esempio semplice:

```Clojure
(str "Ciao," " come" " va?")
;; => "Ciao, come va?"
```

Per concatenare una collezione di stringhe, usiamo `apply`:

```Clojure
(apply str ["Buon" " giorno" "!"])
;; => "Buon giorno!"
```

Se si uniscono stringhe all'interno di un ciclo, `clojure.string/join` può essere utile:

```Clojure
(clojure.string/join " " ["Ciao" "mondo"])
;; => "Ciao mondo"
```

## Approfondimento
Historicamente, la concatenazione di stringhe nei linguaggi di programmazione è stata sempre basilare, ma può essere costosa in termini di performance poiché molti linguaggi creano una nuova stringa piuttosto che modificare quella esistente. In Clojure, la concatenazione è progettata per essere veloce e semplice grazie a immutabilità delle stringhe e la natura funzionale del linguaggio.

Alternative come `StringBuilder` in Java esistono per la manipolazione più efficiente delle stringhe, ma sono raramente necessarie in Clojure a meno che non si stia lavorando con quantità immense di dati.

Internamente, quando usi `str` per concatenare, Clojure compone le stringhe in maniera ottimizzata, preservando l'efficienza. La funzione `apply` è utilizzata per aprire una collezione e passare i suoi elementi come argomenti separati a `str`.

## Vedi Anche
- La documentazione ufficiale di Clojure su [str](https://clojuredocs.org/clojure.core/str) e [apply](https://clojuredocs.org/clojure.core/apply)
- Un'esplorazione dettagliata delle stringhe in Clojure sul [Clojure for the Brave and True](https://www.braveclojure.com/do-things/)
- Una discussione sulle performance di concatenazione stringhe su Clojure's Google Group, [qui](https://groups.google.com/forum/#!topic/clojure/U0BxJtyJ42U)
