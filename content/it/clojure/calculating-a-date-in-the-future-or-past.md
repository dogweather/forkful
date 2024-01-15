---
title:                "Calcolare una Data nel Futuro o nel Passato"
html_title:           "Clojure: Calcolare una Data nel Futuro o nel Passato"
simple_title:         "Calcolare una Data nel Futuro o nel Passato"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile in molte situazioni, come nel caso di pianificazione di eventi, gestione del tempo, o analisi di dati temporali. Usando Clojure, un linguaggio di programmazione funzionale e dinamico, è possibile scrivere codice efficiente e preciso per eseguire queste operazioni.

## Come Fare

Per calcolare una data nel futuro o nel passato usando Clojure, è necessario conoscere la sintassi per la manipolazione delle date e delle ore. Clojure ha una libreria standard chiamata "java.time" che fornisce molte funzioni utili per operare su date e ore.

```Clojure
(ns myapp.core
  (:import [java.time LocalDate]
           [java.time.format DateTimeFormatter]))

;; Definiamo una data di riferimento.
(def date (java.time.LocalDate/now))

;; Calcoliamo una data nel futuro aggiungendo 2 mesi alla data di riferimento.
(def future-date (.plusMonths date 2))

;; Calcoliamo una data nel passato sottraendo 1 anno alla data di riferimento.
(def past-date (.minusYears date 1))

;; Formattiamo le date nel formato "anno-mese-giorno".
(def formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd"))
(def formatted-future (.format future-date formatter))
(def formatted-past (.format past-date formatter))

;; Stampiamo le date calcolate.
(println "Data nel futuro:" formatted-future)
(println "Data nel passato:" formatted-past)

;; Output:
;; Data nel futuro: 2021-08-24
;; Data nel passato: 2019-06-24
```

## Approfondimento

Nel codice di esempio abbiamo usato le funzioni "plusMonths" e "minusYears" per calcolare una data nel futuro o nel passato, ma Clojure offre molte altre funzioni per gestire date e ore. Alcune di queste funzioni sono "of", "get", "with", "truncatedTo" e "isAfter". È possibile consultare la documentazione ufficiale di Clojure per ulteriori informazioni su queste funzioni e sull'utilizzo della libreria "java.time".

## Vedi Anche

- Documentazione ufficiale di Clojure su date e ore: https://clojure.org/reference/java_interop#date_and_time
- Tutorial sulla libreria "java.time": https://cognitect.com/blog/2016/6/1/date-and-time-in-java-or-not