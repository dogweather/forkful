---
title:                "Ottenere la data corrente"
html_title:           "Clojure: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Se stai scrivendo un programma che richiede il calcolo della data attuale, allora devi capire come ottenere questa informazione. Fortunatamente, in Clojure, questo è un compito abbastanza semplice grazie alla sua libreria standard.

## Come fare

Per ottenere la data attuale in Clojure, possiamo utilizzare la funzione `now` della libreria `clojure.java-time`. Questa funzione restituirà un oggetto `java.time.Instant` che rappresenta il momento attuale nel fuso orario UTC. Per convertire questa istanza in un oggetto `java.time.LocalDate` che rappresenta solo la data, possiamo chiamare la funzione `to-local-date`.

```
(use 'clojure.java-time)

(def current-date (-> (now) to-local-date))

(println current-date)
; => #object[java.time.LocalDate 0x468e79b1 "2021-02-25"]
```

La stampa del risultato ci mostra che il nostro nuovo oggetto `current-date` contiene la data attuale nel formato aaaa-MM-dd.

## Approfondimento

Per avere una maggiore precisione sulla data, possiamo specificare anche il fuso orario che ci interessa. In questo caso, possiamo utilizzare la funzione `now-zone` invece di `now`, specificando come argomento il nome del fuso orario desiderato.

Ad esempio, per ottenere la data attuale nell'Italia centrale, possiamo scrivere:

```
(def current-date (-> (now-zone "Europe/Rome") to-local-date))

(println current-date)
; => #object[java.time.LocalDate 0x1c8d6132 "2021-02-25"]
```

Possiamo anche utilizzare la funzione `now-local-date` per ottenere direttamente la data attuale senza dover convertire l'`Instant` in un `LocalDate`.

```
(def current-date (now-local-date))

(println current-date)
; => #object[java.time.LocalDate 0x5f868679 "2021-02-25"]
```

È inoltre possibile formattare la data in un formato specifico utilizzando la funzione `formatter` e la funzione `format` della libreria `clj-time`.

```
(require '[clj-time.format :as fmt])

(def current-date (now-local-date))

(def formatter (fmt/formatter "dd MMMM y"))
(println (fmt/format current-date formatter))
; => 25 febbraio 2021
```

## Vedi anche

- [Documentazione ufficiale di clojure.java-time](https://clojure.github.io/java-time)
- [Guida di Clojure sulle date e orari](https://clojure.org/guides/date_time)