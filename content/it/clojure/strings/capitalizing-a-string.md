---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:51.037331-07:00
description: "Come fare: Clojure, essendo un linguaggio JVM, ti permette di utilizzare\
  \ direttamente i metodi String di Java. Ecco un esempio base di come capitalizzare\u2026"
lastmod: '2024-03-13T22:44:43.026234-06:00'
model: gpt-4-0125-preview
summary: Clojure, essendo un linguaggio JVM, ti permette di utilizzare direttamente
  i metodi String di Java.
title: Capitalizzare una stringa
weight: 2
---

## Come fare:
Clojure, essendo un linguaggio JVM, ti permette di utilizzare direttamente i metodi String di Java. Ecco un esempio base di come capitalizzare una stringa in Clojure:

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojure non include una funzione incorporata specificamente per capitalizzare le stringhe, ma come mostrato, è possibile raggiungere facilmente questo risultato combinando le funzioni `clojure.string/upper-case`, `subs` e `str`.

Per una soluzione più concisa e per gestire manipolazioni di stringhe più complesse, potresti rivolgerti a una libreria di terze parti. Una libreria popolare nell'ecosistema di Clojure è `clojure.string`. Tuttavia, al momento del mio ultimo aggiornamento, non offre una funzione `capitalize` diretta oltre a quanto dimostrato con le funzionalità di base di Clojure, quindi il metodo mostrato sopra è il tuo approccio diretto senza dover includere librerie aggiuntive specificamente per la capitalizzazione.

Ricorda, quando lavori con stringhe in Clojure che interagiscono con metodi Java, stai effettivamente lavorando con stringhe Java, permettendoti di sfruttare l'intero arsenale dei metodi String di Java direttamente nel tuo codice Clojure, se necessario.
