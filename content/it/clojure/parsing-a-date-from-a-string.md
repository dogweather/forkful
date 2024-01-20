---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:35:51.446464-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Tradurre una data da stringa a un formato utilizzabile è essenziale per gestirla in programmi. Lo facciamo perché le date sono spesso scambiate e memorizzate come stringhe.

## How to:
Clojure usa la libreria `clj-time` basata su Joda-Time per questa operazione. Diamo un'occhiata:

```clojure
;; Aggiungi clj-time al tuo progetto
(:require [clj-time.format :as fmt])

;; Definisci un parser usando un pattern
(def custom-formatter (fmt/formatter "dd-MM-yyyy"))

;; Esempio di parsing
(defn parse-date [date-string]
  (fmt/parse custom-formatter date-string))

(parse-date "23-03-2023")
;; => #object[org.joda.time.DateTime 0x ... "2023-03-23T00:00:00.000Z"]
```

## Deep Dive
Clojure, essendo un dialetto di Lisp sulla JVM, sfrutta le librerie Java per il parsing delle date. `clj-time`, storico ma ora in mode legacy, si appoggia su Joda-Time. Tuttavia, Java 8 introdusse `java.time`, che è meglio integrato e considerato il sostituto moderno. Per passare a `java.time`:

```clojure
(:import [java.time.format DateTimeFormatter]
         [java.time LocalDate])

;; Definisci un parser usando `java.time`
(def custom-formatter (DateTimeFormatter/ofPattern "dd-MM-yyyy"))

;; Esempio di parsing
(defn parse-date [date-string]
  (LocalDate/parse date-string custom-formatter))

(parse-date "23-03-2023")
;; => #object[java.time.LocalDate 0x ... "2023-03-23"]
```

`clj-time` è facile da usare, ma `java.time` è più moderno. Nella pratica, scegli quello che si adatta meglio alle tue necessità.

## See Also
- [clj-time GitHub page](https://github.com/clj-time/clj-time)
- [informazioni su Joda-Time](http://www.joda.org/joda-time/)
- [java.time package summary](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Clojure for the Brave and True: [Manipolare le date e l'orario](https://www.braveclojure.com/clojure-for-the-brave-and-true/)