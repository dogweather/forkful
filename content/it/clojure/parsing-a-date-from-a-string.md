---
title:                "Il parsing di una data da una stringa"
html_title:           "Clojure: Il parsing di una data da una stringa"
simple_title:         "Il parsing di una data da una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Parsing di una data da una stringa si riferisce al processo di convertire una data in formato testo in un formato di dati più adatto per l'elaborazione da parte di un computer. I programmatori spesso devono eseguire questa operazione quando lavorano con dati che includono informazioni sulla data in formati diversi.

## Come fare:

Per eseguire il parsing di una data da una stringa in Clojure, è possibile utilizzare la funzione `java.util.Date/parse` o la libreria `clj-time`. Ecco un esempio di codice utilizzando `java.util.Date/parse`:

```Clojure
(def data (java.util.Date/parse "yyyy-MM-dd" "2021-01-31"))
(println data)
```

Output: `Sun Jan 31 00:00:00 EET 2021`

Esempio di codice utilizzando `clj-time`:

```Clojure
(require '[clj-time.format :as f])
(require '[clj-time.core :as t])

(def formato (f/formatter "yyyy-MM-dd"))
(def data (f/parse formato "2021-01-31"))
(println data)
```

Output: `#clj-time.coerce/date "2021-01-31T00:00:00.000-00:00"`

## Approfondimento:

Il parsing di una data da una stringa è un'operazione comune nei linguaggi di programmazione e ha una lunga storia nel campo dell'informatica. Alcune alternative a Clojure per eseguire questa operazione includono Java, Python e JavaScript.

Per quanto riguarda i dettagli di implementazione, è importante comprendere il formato della stringa che contiene la data e il formato di uscita desiderato. Inoltre, è possibile utilizzare le funzioni di formattazione di stringhe per gestire le date in modo più personalizzato.

## Vedi anche:

- [java.util.Date JavaDoc] (https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [clj-time repository] (https://github.com/clj-time/clj-time)