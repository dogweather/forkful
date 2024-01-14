---
title:                "Clojure: Die aktuelle Datum ermitteln"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das aktuelle Datum ist ein wesentlicher Bestandteil der meisten Anwendungen, sei es für die Anzeige von Timern oder für die Verwaltung von Terminen. Deshalb ist es wichtig zu wissen, wie man in Clojure das aktuelle Datum abrufen kann.

## Wie geht das?

In Clojure gibt es eine spezielle Funktion namens `java.util.Date`, welche das aktuelle Datum und die aktuelle Uhrzeit zurückgibt. Diese Funktion muss importiert werden, bevor sie verwendet werden kann. Schauen wir uns ein Beispiel an:

```Clojure
(ns my-app.core
  (:import [java.util Date]))
  
(defn get-current-date []
  (let [current-date (Date.)]
    (println "Das aktuelle Datum ist:" (.toString current-date))))
```

Der obige Code importiert die `java.util.Date` Funktion und definiert dann eine Funktion `get-current-date`, die das aktuelle Datum abruft und ausgibt. Führt man diesen Code aus, wird die aktuelle Datum und Zeit im Format `yyyy-mm-dd hh:mm:ss` ausgegeben.

## Tiefergehend

Es gibt noch einige andere Möglichkeiten, um das aktuelle Datum in Clojure abzurufen. Eine davon ist die Verwendung der `java.time.LocalDateTime` Funktion, die in Java 8 eingeführt wurde. Diese Funktion bietet mehr Flexibilität beim Arbeiten mit Datum und Zeit. Hier ist ein Beispiel:

```Clojure
(ns my-app.core
  (:import [java.time LocalDateTime]))
  
(defn get-current-date []
  (let [current-date (LocalDateTime/now)]
    (println "Das aktuelle Datum ist:" (.toString current-date))))
```

Dieser Code importiert die `java.time.LocalDateTime` Funktion und ruft dann die `now` Funktion auf, um das aktuelle Datum und die Zeit zurückzugeben. Auch hier wird das Datum im Format `yyyy-mm-dd hh:mm:ss` ausgegeben.

## Siehe auch

- [Clojure-Dokumentation zu `java.util.Date`](https://clojuredocs.org/clojure.java.api#clojure.java.api/clojure.lang.IDeref/javagive) 
- [Java-Dokumentation zu `java.time.LocalDateTime`](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)