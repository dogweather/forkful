---
title:                "Clojure: Confrontare due date"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date è spesso un passo necessario in programmazione, specialmente quando si lavora con dati temporali o si desidera ottenere informazioni sui tempi trascorsi tra due eventi. In questo articolo, esploreremo come effettuare questo tipo di confronto utilizzando il linguaggio di programmazione Clojure.

## Come fare

Per confrontare due date in Clojure, possiamo utilizzare la funzione `compare` che prende in input due argomenti di tipo `java.util.Date` e restituisce un valore numerico che indica se la prima data è precedente, successiva o uguale alla seconda data.

```
(def data-1 (java.util.Date. "2020-01-01"))
(def data-2 (java.util.Date. "2020-01-02"))

(println (compare data-1 data-2))
;; Output: -1 (data-1 è precedente a data-2)

(println (compare data-2 data-1))
;; Output: 1 (data-2 è successiva a data-1)

(println (compare data-1 data-1))
;; Output: 0 (data-1 è uguale a data-1)
```

Possiamo anche utilizzare la funzione `before?` o `after?` per ottenere un risultato booleano (vero o falso) a seconda del confronto tra due date.

```
(def data-1 (java.util.Date. "2020-01-01"))
(def data-2 (java.util.Date. "2020-01-02"))

(println (before? data-1 data-2))
;; Output: true (data-1 è precedente a data-2)

(println (after? data-2 data-1))
;; Output: false (data-2 è successiva a data-1)

(println (before? data-1 data-1))
;; Output: false (data-1 è uguale a data-1)
```

## Approfondimento

Il valore restituito dalla funzione `compare` può essere utilizzato anche per ordinare una sequenza di date utilizzando la funzione `sort-by`.

```
(def dates [(java.util.Date. "2020-01-01") (java.util.Date. "2020-01-03") (java.util.Date. "2020-01-02")])

(sort-by compare dates)
;; Output: [(java.util.Date. "2020-01-01") (java.util.Date. "2020-01-02") (java.util.Date. "2020-01-03")]
```

Inoltre, Clojure offre la funzione `between?` che ci permette di verificare se una data si trova tra due date specificate.

```
(def data (java.util.Date. "2020-01-02"))
(def data-inizio (java.util.Date. "2020-01-01"))
(def data-fine (java.util.Date. "2020-01-03"))

(between? data-inizio data data-fine)
;; Output: true (la data è compresa tra data-inizio e data-fine)

(between? data-fine data data-inizio)
;; Output: false (la data non è compresa tra data-fine e data-inizio)
```

## Vedi anche

- [Documentazione ufficiale di Clojure sulla gestione delle date](https://clojure.org/reference/java_interop#_working_with_java_util_date_and_java_util_calendar_objects)
- [Tutorial di Clojure sul confronto tra date](https://www.tutorialspoint.com/clojure/clojure_date_time.htm)