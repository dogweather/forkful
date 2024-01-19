---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Ottenere la data corrente in Clojure

## Cosa & Perché?

Ottenere la data corrente significa determinare la data e l'ora presenti nel momento dell'esecuzione del codice. I programmatori lo fanno per registrare eventi, calcolare il tempo trascorso o gestire operazioni basate su date.

## Come Fare:

In Clojure, per ottenere la data corrente, utilizziamo la funzione `java.util.Date`.

```Clojure
(import 'java.util.Date)
(defn current-date []
  (Date.))
```

Eseguendo la funzione `current-date`, otteniamo:

```Clojure
(current-date)
=> #<Date Sat Nov 27 00:00:00 CEST 2021>
```

## Approfondimento

### Contesto storico
Clojure, essendo una moderna variante di Lisp che gira sulla JVM, ha accesso a tutte le librerie Java. Quindi, per ottenere la data corrente, utilizza l'oggetto `java.util.Date` di Java.

### Alternative
Un'altra opzione è utilizzare la libreria Joda-Time che fornisce una gestione più completa delle date e degli orari.

```Clojure
(ns my-app.time
  (:require [clj-time.core :as time]))

(defn current-date []
  (time/now))
```

### Dettagli implementativi
`java.util.Date` restituisce la data e l'ora correnti in millisecondi da mezzanotte del 1° gennaio 1970 GMT, una convenzione molto comune nella programmazione denominata "Epoch Time" o "Unix Time".

## Vedere Anche

1. [Clojure - Date Time](https://www.tutorialspoint.com/clojure/clojure_date_time.htm)
2. [Joda-Time with Clojure](https://andrewclements.me/2013/05/joda-time-with-clojure/)
3. [Unix Time - Wikipedia](https://it.wikipedia.org/wiki/Tempo_UNIX)