---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:48:53.798172-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generare numeri casuali è come tirare una moneta virtuale: non sai mai su che lato cadrà. I programmatori li usano per tutto, dalla crittografia ai giochi, alla simulazione di dati per i test.

## How to:
Usa `rand` per un numero tra 0 e 1, `rand-int` per un intero:

```Clojure
; Numero casuale tra 0 e 1
(rand)

; Numero intero casuale inferiore a 10
(rand-int 10)
```
Proviamo:

```Clojure
(rand)      ; => 0.7094088652243345 (ogni volta sarà diverso)
(rand-int 10) ; => 7 (ogni volta sarà diverso)
```

Per range specifici:

```Clojure
; Numero casuale tra 10 e 20
(+ 10 (rand-int 11))
```

## Deep Dive:
La generazione di numeri casuali in Clojure sfrutta Java, la sua piattaforma ospitante. In passato, i numeri pseudo-casuali potevano essere prevedibili, ma oggi gli algoritmi sono robusti. Potresti incontrare `java.util.Random` o `java.security.SecureRandom` sotto il cofano. Se hai bisogno di alternative, guarda le librerie come `test.check` per generatori di numeri avanzati o usa direttamente Java se Clojure non ha quello che ti serve.

## See Also:
- [Clojure Documentation](https://clojure.org)
- [Clojure for the Brave and True](https://www.braveclojure.com/)
- [test.check: A Property-based Testing Library](https://github.com/clojure/test.check)