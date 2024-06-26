---
date: 2024-01-26 04:39:10.955177-07:00
description: "Come fare: Clojure fornisce un supporto integrato per i numeri complessi\
  \ tramite la classe di utilit\xE0 `clojure.lang.Numbers`. Usa `complex` per creare\u2026"
lastmod: '2024-03-13T22:44:43.036000-06:00'
model: gpt-4-0125-preview
summary: "Clojure fornisce un supporto integrato per i numeri complessi tramite la\
  \ classe di utilit\xE0 `clojure.lang.Numbers`."
title: Lavorare con i numeri complessi
weight: 14
---

## Come fare:
Clojure fornisce un supporto integrato per i numeri complessi tramite la classe di utilità `clojure.lang.Numbers`. Usa `complex` per creare numeri complessi ed eseguire operazioni aritmetiche.

```clojure
;; Creazione di numeri complessi
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; Addizione
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; Sottrazione
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; Moltiplicazione
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; Divisione
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; Coniugato
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## Approfondimento
I numeri complessi sono stati formalizzati dai matematici come Gauss ed Euler nel XVIII secolo. Sebbene inizialmente accolti con scetticismo, da allora sono diventati fondamentali nella scienza e ingegneria moderna. Clojure non ha un tipo nativo di numero complesso come alcune lingue (ad esempio, Python), ma l'interoperabilità Java inclusa può gestire le operazioni necessarie tramite la classe `clojure.lang.Numbers`.

`java.lang.Complex` di Java è un'alternativa solida, che fornisce più funzionalità e potenziali ottimizzazioni. L'interoperabilità con l'host di Clojure semplifica il lavoro con le librerie Java.

Sotto il cofano, l'aritmetica dei numeri complessi coinvolge l'aggiunta e la moltiplicazione delle parti reale e immaginaria, con la regola chiave che `i^2 = -1`. La divisione di numeri complessi può essere più complicata, tipicamente richiede il coniugato per evitare la divisione per numeri complessi.

## Vedi Anche
- ClojureDocs, per un riferimento rapido: https://clojuredocs.org/
- L'API Java per `java.lang.Complex`: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- La pagina Wikipedia sui numeri complessi per i curiosi di matematica: https://en.wikipedia.org/wiki/Complex_number
