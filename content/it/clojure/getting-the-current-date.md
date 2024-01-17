---
title:                "Ottenere la data corrente."
html_title:           "Clojure: Ottenere la data corrente."
simple_title:         "Ottenere la data corrente."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Ottenere la data corrente è una funzione molto comune nei linguaggi di programmazione, anche in Clojure. Molto spesso, i programmatori hanno bisogno di sapere la data attuale per registrare l'orario delle operazioni o per generare rapporti o statistiche. In breve, ottenere la data corrente è un'operazione molto utile per i programmatori.

## Come lo si fa:

```
Clojure (def current-date (java.util.Date.)) (println current-date)
```

Output: Wed Apr 21 18:21:45 CEST 2021 

In questo esempio, utilizziamo la funzione java.util.Date per ottenere la data corrente e la assegniamo alla variabile current-date. Successivamente, utilizziamo la funzione println per stampare questa variabile, che ci darà l'output desiderato.

## Approfondimento:

Per ottenere la data corrente, Clojure utilizza la libreria Java java.util.Date, che offre diverse funzioni per manipolare e stampare la data e l'ora attuali. Tuttavia, esistono anche altre alternative come la libreria datetime, che offre funzionalità più avanzate per la gestione della data e dell'ora.

## Vedi anche:

- [Documentazione ufficiale di Clojure sulla funzione java.util.Date] (https://clojure.org/api/java.util.Date)
- [Libreria datetime per Clojure] (https://github.com/clj-time/clj-time)