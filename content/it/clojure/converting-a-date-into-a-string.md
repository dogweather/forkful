---
title:                "Clojure: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte situazioni in cui potresti voler convertire una data in una stringa nel tuo codice Clojure. Ad esempio, potresti dover visualizzare la data in un formato specifico per l'utente o salvarla in un formato compatibile con un'altra applicazione.

## Come fare
Per convertire una data in una stringa in Clojure, puoi utilizzare la funzione `format` del pacchetto `java.time`. Questa funzione accetta due argomenti: una stringa di formattazione e la data da convertire. Ecco un esempio di codice che converte la data corrente in una stringa nel formato "GG/MM/AAAA":

```Clojure
(require '[java.time :as time])

(def current-date (time/local-date))
(def formatted-date (time/format "dd/MM/yyyy" current-date))

(formatted-date) ;; output: "04/01/2022"
```

## Approfondimento
La stringa di formattazione che si passa alla funzione `format` segue le stesse regole della classe `SimpleDateFormat` di Java. Puoi utilizzare una combinazione di lettere per definire il formato della data, come "GG" per il giorno, "MM" per il mese e "AAAA" per l'anno. Inoltre, puoi aggiungere del testo in qualsiasi punto della stringa di formattazione, ad esempio "/", per separare le parti della data.

See Also
- [Documentazione ufficiale di java.time](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Guida alla formattazione delle date in Clojure](https://clojure.org/guides/date_time)
- [Tutorial di Clojure per principianti](https://www.clojurebook.com)