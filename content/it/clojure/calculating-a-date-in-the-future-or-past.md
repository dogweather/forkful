---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Clojure: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Calcolare una data nel futuro o nel passato è una pratica comune che permette di manipolare oggetti temporali. Lo facciamo per diverse ragioni, dalle funzioni di pianificazione ai tracker di eventi.

## Come fare:
L'aggiunta o la sottrazione di giorni a una data è piuttosto semplice in Clojure. Ecco un semplice esempio:

```clojure
(ns example.core
  (:require [java-time :as jt]))

(defn add-days [date days]
  (jt/plus date (jt/days days)))

(defn sub-days [date days]
  (jt/minus date (jt/days days)))

(defn -main []
  (let [current-date (jt/local-date)
        future-date (add-days current-date 20)
        past-date (sub-days current-date 10)]
    (println "Oggi è: " current-date)
    (println "Tra 20 giorni sarà: " future-date)
    (println "10 giorni fa era: " past-date)))
```
L'output del campione può essere il seguente:
```clojure
Oggi è: 2032-12-10
Tra 20 giorni sarà: 2032-12-30
10 giorni fa era: 2032-11-30
```

## Deep Dive
Historicamente, il calcolo delle date era un processo più complesso. I programmatori dovevano fare i conti con le sfide come gli anni bisestili e le differenze tra i vari calendari. Clojure, come molti moderni linguaggi di programmazione, utilizza la libreria Java-Time che standardizza queste operazioni.

Esistono alternative alla libreria Java-Time, come la libreria clj-time. Tuttavia, è generalmente consigliato utilizzare Java-time dato che è la libreria più aggiornata e supporta tutte le funzioni di Java 8.

Dal punto di vista dell'implementazione, sia jt/plus che jt/minus creano una nuova istanza dell'oggetto data con la nuova data calcolata. Questo è dovuto all'immuteabilità delle date in Clojure, una caratteristica che aiuta a prevenire errori inaspettati nel codice.

## Guarda Anche
Per maggiori dettagli sulla manipolazione delle date in Clojure, vedi: