---
date: 2024-01-20 17:36:31.224420-07:00
description: "Convertire una data in una stringa trasforma il binario comprensibile\
  \ solo da computer in un formato facilmente leggibile da esseri umani. I programmatori\u2026"
lastmod: '2024-03-13T22:44:43.053949-06:00'
model: gpt-4-1106-preview
summary: "Convertire una data in una stringa trasforma il binario comprensibile solo\
  \ da computer in un formato facilmente leggibile da esseri umani. I programmatori\u2026"
title: Conversione di una data in una stringa
---

{{< edit_this_page >}}

## What & Why?
Convertire una data in una stringa trasforma il binario comprensibile solo da computer in un formato facilmente leggibile da esseri umani. I programmatori fanno questo per visualizzare date in log, interfacce utente, e documenti.

## How to:
Usa `java.text.SimpleDateFormat` e `java.util.Date` per convertire date. Ecco un esempio:

```Clojure
(import '[java.text SimpleDateFormat]
        '[java.util Date])

(defn format-date [date pattern]
  (let [formatter (SimpleDateFormat. pattern)]
    (.format formatter date)))

(println (format-date (Date.) "dd/MM/yyyy"))
```

Output di esempio:
```
31/03/2023
```

## Deep Dive
Clojure, essendo un dialetto di Lisp che gira su JVM, sfrutta le librerie Java per la manipolazione delle date. Sebbene sia possibile usare librerie Java native come `SimpleDateFormat`, Clojure offre anche librerie come `clj-time` che è una wrapper della libreria Joda-Time, fornendo un'interfaccia più idiomatica.

Prima dell'adozione di Joda-Time e poi `java.time`, programmatori spesso riscontravano difetti e limitazioni nell'API data/ora di Java.

Rispetto a `SimpleDateFormat`, che è mutabile e non thread-safe, considera di usare `java.time.format.DateTimeFormatter` se sei su Java 8 o versioni successive:

```Clojure
(import '[java.time.format DateTimeFormatter]
        '[java.time LocalDateTime])

(defn format-date-java8 [date pattern]
  (let [formatter (DateTimeFormatter/ofPattern pattern)]
    (.format date formatter)))

(println (format-date-java8 (LocalDateTime/now) "dd/MM/yyyy"))
```

L'alternativa per la conversione di date in Clojure è più espressiva e direttamente supportata nell'ecosistema.

## See Also
- Clojure's `clj-time` library: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Java's `java.time` package (Java 8 and later): [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Guida ufficiale Clojure: [https://clojure.org/guides/destructuring](https://clojure.org/guides/destructuring)
- Documentazione `SimpleDateFormat`: [https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
