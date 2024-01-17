---
title:                "Confrontare due date"
html_title:           "Clojure: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Comparare due date è un'operazione comune in programmazione per confrontare la precedenza o l'ordine temporale delle date. I programmatori lo fanno per gestire compiti come la pianificazione, la gestione delle scadenze o la creazione di log temporali.

## Come si fa:

```Clojure
(def data1 (java.util.Date.)) ; crea un oggetto java.util.Date per la data corrente
(def data2 (java.util.Date. 2018 5 20)) ; crea un oggetto java.util.Date per il 20 maggio 2018 
(.compareTo data1 data2) ; restituisce -1 se data1 è precedente a data2, 0 se sono uguali, 1 se data2 è precedente a data1
(.before data1 data2) ; restituisce true se data1 è prima di data2
(.after data2 data1) ; restituisce true se data2 è dopo data1
; è possibile utilizzare anche funzioni di terze parti come clj-time per semplificare il confronto di date
(require '[clj-time.core :as time])
(time/after? data1 data2) ; restituisce true se data1 è successiva a data2
```

## Approfondimento:

In passato, prima dell'avvento delle librerie come clj-time, il confronto di date in Clojure richiedeva l'utilizzo delle funzioni della libreria Java.util.Date. Oltre a ciò, in alcuni casi si può considerare l'uso della libreria Joda-Time, che offre funzionalità più avanzate per il confronto di date e la gestione del fuso orario. Inoltre, è importante prestare attenzione alle differenze tra i tipi di dati di Java e Clojure nel manipolare le date.

## Vedi anche:

- [The Clojure Cheatsheet: Date and Time](https://clojure.org/api/cheatsheet#_date_and_time)
- [Java.util.Date documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Joda-Time documentation](https://www.joda.org/joda-time/)