---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?
Comparare due date significa determinare se una data è precedente, successiva o uguale a un'altra. Questo è utile per gli sviluppatori quando si devono eseguire operazioni mirate nel tempo, come ad esempio programmare degli eventi o ordinare dati cronologicamente.

## Come si fa:
Ecco un esempio su come comparare due date in Clojure:

```Clojure
(ns clojure.examples
 (:require [clj-time.core :as t]
           [clj-time.coerce :as c]))
  
(defn compare-dates [date1 date2]
 (t/compare 
  (c/to-local-date date1) 
  (c/to-local-date date2)))
```
Uso dell'esempio:

```Clojure
;; date1 è 2022-11-11
;; date2 è 2022-11-12
(compare-dates "2022-11-11" "2022-11-12") 
;; output: -1
;; indica che la data1 è prima di data2
```

## Approfondimento:
Storicamente, nelle prime versioni di Clojure, non era così facile comparare due date. La funzionalità è stata introdotta più tardi con il pacchetto `clj-time`. 

Esistono metodi alternativi per comparare le date in Clojure. Ad esempio, `java.time` (disponibile dal JDK 8) può essere utilizzato in alternativa a `clj-time`.

I dettagli di implementazione della funzione `compare` di Clojure possono essere complessi. In sintesi, il metodo `compare` di Java viene utilizzato internamente per fornire la funzionalità di comparazione delle date.

## Vedi Anche:
- La documentazione ufficiale di Clojure: [https://clojure.org/](https://clojure.org/)
- Un'utile guida passo-passo sulla libreria clj-time: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- La documentazione ufficiale di java.time: [https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html)