---
title:                "Clojure: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Sei curioso di sapere come ottenere la data corrente utilizzando il linguaggio di programmazione Clojure? Continua a leggere per scoprire come!

## Come Fare

Per ottenere la data corrente in Clojure, puoi utilizzare la funzione `now` dal modulo `java.time.LocalDateTime`. Ecco un esempio di come utilizzarla:

```Clojure
(require '[java.time.LocalDateTime :as dt])

(def current-date (dt/now))
(println current-date)
```

Questo dovrebbe restituire l'output della data e dell'ora correnti nel seguente formato: `2021-08-20T15:30:00.000`. Puoi anche utilizzare la funzione `now` dal modulo `java.time.LocalDate` per ottenere solo la data, senza l'ora.

Se vuoi personalizzare il formato dell'output, puoi utilizzare la funzione `format` dal modulo `java.time.format.DateTimeFormatter`. Ad esempio, se vuoi ottenere la data nel formato giorno/mese/anno, puoi utilizzare il seguente codice:

```Clojure
(require '[java.time.LocalDate :as ld])
(require '[java.time.format.DateTimeFormatter :as fmt])

(def current-date (ld/now))
(def formatted-date (fmt/format current-date "dd/MM/yyyy"))
(println formatted-date)
```

Questo dovrebbe restituire l'output della data corrente nel formato richiesto, ad esempio `20/08/2021`.

## Approfondimento

Per ottenere la data corrente in Clojure, il linguaggio sfrutta la potenza della libreria Java `java.time`. Questa libreria fornisce diverse classi e metodi per gestire date, orari e fusi orari in modo semplice e intuitivo. Se vuoi approfondire ulteriormente l'utilizzo di questa libreria, puoi consultare la documentazione ufficiale di Java o cercare online ulteriori tutorial specifici su Clojure.

## Vedi Anche

- [Documentazione ufficiale di Java - java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Tutorial per principianti su Clojure](https://clojure.org/guides/getting_started)
- [Altri articoli su Clojure in Italiano](https://www.adaltas.com/en/docker/docker-get-local-datetime-node/)

Scoprire come ottenere la data corrente in Clojure può sembrare un dettaglio banale, ma è un'abilità fondamentale per ogni programmatore. Utilizzando la libreria `java.time`, puoi gestire facilmente date e orari nei tuoi progetti Clojure. Speriamo che questo articolo ti sia stato utile e ti abbia dato una maggiore comprensione di come ottenere la data corrente in Clojure. Buon codice!