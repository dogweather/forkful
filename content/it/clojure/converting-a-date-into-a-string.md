---
title:                "Convertire una data in una stringa"
html_title:           "Clojure: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler convertire una data in una stringa in Clojure. Ad esempio, potresti dover rappresentare una data in un formato specifico per motivi di visualizzazione o per l'elaborazione dei dati.

## Come fare

Per convertire una data in una stringa in Clojure, puoi utilizzare la funzione `format` del modulo `clojure.core`. Di seguito un esempio di codice che utilizza questa funzione per convertire una data in una stringa nel formato "giorno/mese/anno":

```Clojure
(require '[clojure.core :as core])
(def data (java.util.Date.))
(core/format data "dd/MM/yyyy")
```

L'output di questo codice sarà "10/11/2021" se viene eseguito il 10 novembre 2021.

## Approfondimento

Per una maggiore flessibilità, puoi anche utilizzare la libreria `clj-time` per gestire date e orari in Clojure. Questa libreria fornisce funzioni utili per manipolare, formattare e convertire date e orari. Inoltre, è possibile specificare il fuso orario desiderato nella conversione di una data in una stringa, utilizzando la funzione `with-timezone` del modulo `clj-time.core`.

```Clojure
(require '[clj-time.core :as time]
(def data (java.util.Date.))
(time/format (time/with-timezone data "UTC+02:00") "dd/MMM/yyyy")
```

L'output di questo codice sarà "10/nov/2021", utilizzando il fuso orario UTC+02:00.

## Vedi anche

- Documentazione ufficiale di `clojure.core/format`: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/format
- Documentazione ufficiale di `clj-time`: https://clj-time.github.io/clj-time/