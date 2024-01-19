---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Umwandeln eines Datums in einen String ist eine Methode, um ein Datum in einen lesbaren Text zu konvertieren. Dies hilft Programmierern dabei, Daten effizient zu speichern, zu manipulieren und Logdateien zu erstellen.

## So geht's:

Wir können die Java SimpleDateFormat Bibliothek direkt in Clojure nutzen. In Clojure haben wir eine eingebaute Funktion namens `clj-time.format/to-string`.

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.format :as f])

(def my-date (t/date-time 2020 12 25)) ;; Unser Datum zum Umwandeln

;; Konvertieren Sie das Datum in einen String mithilfe der ISO Datumsformatierung
(def my-string (f/to-string my-date))

;; Gibt "2020-12-25T00:00:00.000Z" zurück
```

## Tiefer Eintauchen:

**Historischer Kontext:** Im Laufe der Zeit verlagerten sich die Sprachen wie Java nach Clojure wegen seiner funktionalen Einfachheit. Die SimpleDateFormat-Funktion wurde von Java portiert.

**Alternativen:** Sie können auch die Java SimpleDateFormat Methode verwenden, wenn Sie ein spezielles Datumsformat haben, das Sie benötigen:

```Clojure
(require '[clj-time.coerce :as c])

(def custom-format (java.text.SimpleDateFormat. "yyyy-MM-dd"))

(def date-string (.format custom-format (c/to-date my-date)))
```

**Implementierungsdetails:** Die `to-string` Funktion in `clj-time.format` verwendet intern das ISO 8601 Format. Es ist eine sehr allgemeine Funktion, die viele gängige Datumsformate abdeckt. Für speziellere Formate können Sie die Java SimpleDateFormat Klasse verwenden.

## Siehe auch:

1. Clojure clj-time Bibliothek Dokumentation [Link](https://clj-time.github.io/clj-time/)
2. Java SimpleDateFormat Dokumentation [Link](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
3. ISO 8601 Datum Format [Link](https://de.wikipedia.org/wiki/ISO_8601)