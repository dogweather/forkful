---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:13:47.998621-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Ottenere la data corrente in programmazione significa acquisire la data del momento in cui la richiesta viene fatta. I programmatori la usano per gestire eventi basati sul tempo, come scadenze, timestamp o funzioni di schedulazione.

## How to:
In Clojure, usi la libreria Java interop per ottenere la data corrente. Ecco un esempio:

```Clojure
(import 'java.util.Date)

(defn current-date []
  (str (Date.)))
```

Esempio di output:

```
"Fri Apr 07 15:20:46 CEST 2023"
```

Se vuoi solo la data senza l'ora, puoi fare così:

```Clojure
(import 'java.text.SimpleDateFormat)
(import 'java.util.Date)

(defn current-date-only []
  (let [format (SimpleDateFormat. "yyyy-MM-dd")]
    (.format format (Date.))))
```

Output previsto:

```
"2023-04-07"
```

## Deep Dive
Clojure, essendo un dialetto di Lisp che funziona sulla JVM, sfrutta le librerie di Java per la gestione delle date. Historically, `java.util.Date` era il modo per ottenere la data, ma aveva delle questioni. Java 8 ha introdotto il `java.time` package, con una migliore API, ma per brevità questo esempio resta fedele all'uso semplice e diretto di `Date`.

Alternativamente, Clojure ha librerie create proprio per semplificare queste operazioni, come `clj-time`, che è un wrapper su Joda-Time, che predatava `java.time`.

Dettagli di implementazione:
- Clojure fornisce accesso diretto alle classi Java, quindi, non c'è bisogno di reinventare la ruota.
- I dati sono immutabili in Clojure, assicurandosi che l'operazione di ottenere la data corrente non abbia effetti secondari indesiderati.

## See Also
- [Clojure Documentation](https://clojure.org/)
- [java.util.Date Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [SimpleDateFormat Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [clj-time GitHub page](https://github.com/clj-time/clj-time)