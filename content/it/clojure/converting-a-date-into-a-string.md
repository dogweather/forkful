---
title:                "Clojure: Convertire una data in una stringa"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa è spesso necessario quando si lavora con dati temporali in un linguaggio di programmazione, come Clojure. Ciò consente di ottenere una rappresentazione leggibile della data e utilizzarla per scopi come il logging o la visualizzazione.

## Come fare

Per convertire una data in una stringa in Clojure, è possibile utilizzare la funzione `format` del modulo `java.time.format`. Di seguito un esempio di codice che mostra come ottenere una stringa formattata per la data corrente:

```Clojure
(require '[java-time.format :as format])

(def dateFormatter (format/formatter "dd/MM/yyyy"))

(format/formatter dateFormatter
  (java.time.LocalDate/now))
```

Questo codice produrrà una stringa formattata come "26/03/2021", rappresentante la data corrente.

## Approfondimenti

Per capire meglio il processo di conversione di una data in una stringa, è importante conoscere il sistema di formattazione delle date utilizzato in Clojure. La funzione `format/formatter` accetta un parametro opzionale che specifica il pattern di formattazione della data. Questo pattern può includere lettere di formato per rappresentare diversi componenti della data, come giorno, mese, anno, ecc. Inoltre, è possibile specificare un locale per personalizzare il formato della data in base alle convenzioni regionali.

## Vedi anche

- Documentazione ufficiale di `java.time.format` per ulteriori dettagli sulla formattazione delle date in Clojure: https://clojure.github.io/spec.alpha/clojure.spec.alpha.html
- Articolo sulla gestione delle date e dei tempi in Clojure: https://blog.cognitect.com/blog/2016/5/16/clojure-and-time-zones
- Tutorial su come creare e manipolare oggetti `java.time` in Clojure: https://www.braveclojure.com/java-time/