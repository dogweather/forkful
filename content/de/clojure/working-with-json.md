---
title:                "Arbeiten mit JSON"
html_title:           "Clojure: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-json.md"
---

{{< edit_this_page >}}

Beim Programmieren mit Clojure kann es manchmal nötig sein, mit JSON-Daten umzugehen. Aber was ist das überhaupt und warum machen Programmierer das?

## Was & Warum?

JSON steht für "JavaScript Object Notation" und ist ein gängiges Format zum Austausch von Daten zwischen verschiedenen Anwendungen. Es ist lesefreundlicher als z.B. XML und wird von vielen Programmiersprachen unterstützt, einschließlich Clojure. Programmierer nutzen JSON, um Daten effizient zu speichern und weiterzugeben, z.B. in Webanwendungen oder bei der Verarbeitung von APIs.

## Wie geht's?

Um mit JSON in Clojure zu arbeiten, können wir die Bibliothek "clojure.data.json" verwenden. Diese stellt Funktionen zur Verfügung, um JSON-Dateien zu lesen, zu schreiben und zu verarbeiten. Schauen wir uns ein Beispiel an:

```Clojure
(require '[clojure.data.json :as json])

;; JSON-Datei lesen
(def data (json/read-str "{\"name\": \"John\", \"age\": 30}"))

;; JSON-Datei schreiben
(json/write-str {:name "Jane", :age 25})

;; JSON-Datei verarbeiten
(def name (get data "name"))
(def age (get data "age"))
```

## Tief eintauchen

JSON wurde Anfang der 2000er Jahre entwickelt und ist heute eines der am häufigsten verwendeten Datenformate. Eine alternative Möglichkeit, Daten in Clojure zu verarbeiten, ist die Verwendung von EDN (Extensible Data Notation), die in Clojure eingebaut ist. Im Gegensatz zu JSON ist EDN jedoch spezifisch für Clojure und keine universelle Lösung für den Austausch von Daten.

Um mit komplexeren JSON-Strukturen umzugehen, gibt es auch die Möglichkeit, spezielle Bibliotheken wie "cheshire" oder "jsonista" zu verwenden. Diese bieten zusätzliche Funktionen, um z.B. JSON-Schemas zu validieren oder die Integration mit anderen Datenformaten zu erleichtern.

## Siehe auch

- Offizielle Clojure-Dokumentation für die "clojure.data.json" Bibliothek: https://clojuredocs.org/clojure.data.json 
- Artikel über die Unterschiede zwischen JSON und EDN in Clojure: https://practicalli.github.io/blog/posts/edn-and-json-in-clojure/
- Vergleich von verschiedenen JSON-Bibliotheken in Clojure: https://www.brianthicks.com/post/2015/01/31/json-processing-in-clojure/