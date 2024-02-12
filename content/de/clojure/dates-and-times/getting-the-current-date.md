---
title:                "Den aktuellen Datum abrufen"
aliases:
- /de/clojure/getting-the-current-date.md
date:                  2024-02-03T19:09:19.061380-07:00
model:                 gpt-4-0125-preview
simple_title:         "Den aktuellen Datum abrufen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums in der Programmierung ist aus einer Vielzahl von Gründen entscheidend, einschließlich Protokollierung, Zeitstempelung von Ereignissen und Planen von Aufgaben. In Clojure, einem Lisp-Dialekt auf der JVM, nutzt diese Aufgabe die Java-Interop-Fähigkeiten, die einen unkomplizierten Zugang zur umfangreichen Java Date-Time API ermöglichen.

## Wie man es macht:

### Verwendung von Java-Interop
Die nahtlose Interoperabilität von Clojure mit Java ermöglicht es Ihnen, direkt auf die Java Date-Time API zuzugreifen. So können Sie das aktuelle Datum abrufen:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; Beispiel-Ausgabe
(get-current-date) ; "2023-04-15"
```

### Verwendung der clj-time-Bibliothek
Für eine idiomatischere Clojure-Lösung könnten Sie sich für die `clj-time` Bibliothek entscheiden, ein Wrapper um Joda-Time, obwohl für die meisten neuen Projekte die integrierte Java 8 Date-Time API empfohlen wird. Sollten Sie jedoch `clj-time` bevorzugen oder benötigen:

Fügen Sie zunächst `clj-time` zu Ihren Projektabhängigkeiten hinzu. In Ihrer `project.clj` fügen Sie hinzu:

```clojure
[clj-time "0.15.2"]
```

Dann verwenden Sie es, um das aktuelle Datum zu bekommen:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; Beispiel-Ausgabe
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

Beide Methoden bieten schnelle, effektive Wege, um das aktuelle Datum in Clojure zu erhalten, indem sie die Leistungsfähigkeit der zugrundeliegenden Java-Plattform nutzen oder den Komfort einer spezifisch für Clojure entwickelten Bibliothek in Anspruch nehmen.
