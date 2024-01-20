---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Convertire una data in una stringa consiste nell'interpretare un oggetto data come un insieme di caratteri. Questo viene fatto dai programmatori per facilitare la visualizzazione, la memorizzazione o il trasferimento di dati tra diverse tecnologie.

## Come fare:
In Clojure, possiamo sfruttare la funzione built-in `clj-time.format/unparse` del modulo clj-time. Ecco un esempio di come fare:

```clojure
(ns my-app.core
  (:require [clj-time.format :as f]))

(defn date-to-string [date]
  (f/unparse
   (f/formatter "yyyy-MM-dd")
   date))

;; Uso:
(pprint (date-to-string (t/date-time 2022 03 16)))
```

Questo producirà:

```clojure
"2022-03-16"
```

## Approfondimento
La necessità di convertire una data in una stringa risale a quando le tecnologie informatiche hanno iniziato a manipolare dati di questo tipo. In Clojure, `clj-time` è prevalentemente usato per operazioni di data e ora, ispirato alla popolare libreria Joda-Time di Java.

Ci sono anche alternative. Potremmo usare la funzione `java.util.Date.toString()`, ma produce una stringa in un formato meno versatile rispetto a `clj-time`.

In termini di implementazione, `clj-time.format/unparse` converte una data o un'ora in una stringa utilizzando un oggetto `org.joda.time.format.DateTimeFormatter`.

## Leggi Ancora
Per comprendere meglio il lavoro con le date in Clojure, potresti trovare utili queste risorse:

- [Documentazione ufficiale di clj-time](https://clj-time.github.io/clj-time/)