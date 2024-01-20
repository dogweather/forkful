---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Che Cos’è & Perché?

L'analisi di una data da una stringa significa convertire un testo che rappresenta una data in un oggetto data che il tuo programma può manipolare. I programmatori lo fanno per leggere le date da file di testo, base di dati e altre fonti.

## Come si fa:

In Clojure, puoi utilizzare la libreria java.time per analizzare le stringhe di data.

```Clojure
(import 'java.time.LocalDate)
(import 'java.time.format.DateTimeFormatter)

(defn parse-data [data-str]
  (LocalDate/parse data-str (DateTimeFormatter/ofPattern "dd/MM/yyyy")))

(parse-data "15/03/2021")
```

Output:
```Clojure
#object[java.time.LocalDate 0x234f5345 "2021-03-15"]
```

## Una Osservazione Più Profonda

Nonostante l'universalità dell'operazione, l'analisi delle date presenta alcune sfide a causa delle diverse rappresentazioni delle date a seconda della cultura o del formato di data utilizzato. Prima della libreria java.time, era comune in Java utilizzare il più meno intuitivo `java.util.Date` e conversioni di formato con `SimpleDateFormat`.

Come alternativa, esistono le librerie Joda-Time e clj-time, molto ricche e con un'astrazione superiore alle API Java. Comunque, dal momento che Java 8, la libreria java.time preferita dato che è integrata nell'SDK di Java.

L'implementazione di questa funzione in Clojure sfrutta le API di Java e la sua interoperabilità. La funzione `parse-date` richiede una stringa e la formatta in un oggetto LocalDate.

## Per Approfondire

Se vuoi fare pratica con questi concetti, o se sei alla ricerca di una documentazione più dettagliata, ecco alcuni link utili:

- Documentazione della libreria java.time: [Java 8 java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Conversioni di data e ora in Clojure: [Clojure, l'ora giusta](http://blog.joda.org/2021/11/clojure-time-right-time.html)
- Guida per parsare le stringhe di data in Clojure: [Parsing a Date from a String in Clojure](https://stackoverflow.com/questions/31928269/parsing-a-date-from-a-string-in-clojure)