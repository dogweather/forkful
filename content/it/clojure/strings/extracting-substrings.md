---
date: 2024-01-20 17:45:16.064856-07:00
description: "Estrarre sottostringhe significa selezionare parti specifiche da una\
  \ stringa di testo. I programmatori lo fanno per analisi, manipolazione dati o\u2026"
lastmod: '2024-03-13T22:44:43.031589-06:00'
model: gpt-4-1106-preview
summary: Estrarre sottostringhe significa selezionare parti specifiche da una stringa
  di testo.
title: Estrazione di sottostringhe
weight: 6
---

## Cos'è e Perché?
Estrarre sottostringhe significa selezionare parti specifiche da una stringa di testo. I programmatori lo fanno per analisi, manipolazione dati o semplicemente per estrarre informazioni rilevanti.

## Come fare:
```Clojure
;; Estrarre una sottostringa con 'subs'
(def testo "Ciao amici, come va?")
(def sottostringa (subs testo 5 10))
(println sottostringa)  ;; output: amici
```

```Clojure
;; Utilizzare 'clojure.string' per più opzioni
(require '[clojure.string :as str])

;; Estrarre usando 'split' e 'nth'
(def divisione (str/split testo #" "))
(println (nth divisione 1)) ;; output: amici
```

```Clojure
;; Combinare funzioni per risultati complessi
(def inizio (str/index-of testo "amici"))
(def fine (+ inizio (count "amici")))
(println (subs testo inizio fine)) ;; output: amici
```

## Approfondimento
Le funzioni per estrarre sottostringhe esistono da quando sono nate le stringhe nei linguaggi di programmazione. In Clojure, `subs` è la scelta diretta per ottenere sottostringhe. Si potrebbero usare anche espressioni regolari e `split` per casi più complessi. L'efficienza di `subs` si basa sull'implementazione di stringhe Java, quindi è veloce e affidabile. Alternative includono il diretto utilizzo di metodi Java su stringhe Clojure, data la loro interoperabilità.

## Vedi Anche
- [ClojureDocs `subs`](https://clojuredocs.org/clojure.core/subs)
- [Java String Documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
