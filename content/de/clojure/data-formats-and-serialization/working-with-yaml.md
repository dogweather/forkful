---
title:                "Arbeiten mit YAML"
date:                  2024-02-03T19:24:56.732600-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

YAML, ein rekursives Akronym für "YAML Ain't Markup Language", ist ein für Menschen lesbares Serialisierungsformat für Daten, das für Konfigurationsdateien und den Datenaustausch zwischen Sprachen mit unterschiedlichen Datenstrukturen verwendet wird. Programmierer setzen YAML aufgrund seiner Einfachheit und Lesbarkeit ein, was es zu einer idealen Wahl für die Konfiguration von Anwendungen und die Erleichterung des Datenaustauschs in polyglotten Programmierumgebungen macht.

## Wie:

Clojure bietet keine integrierte Unterstützung für YAML, aber Sie können Drittanbieter-Bibliotheken wie `clj-yaml` für das Parsen und Generieren von YAML-Daten nutzen. Fügen Sie zuerst die Bibliothek Ihren Projektabhängigkeiten hinzu:

```clojure
;; Fügen Sie dies Ihren project.clj-Abhängigkeiten hinzu
[clj-yaml "0.7.0"]
```

So können Sie `clj-yaml` verwenden, um YAML zu parsen und Clojure-Maps in YAML umzuwandeln.

### YAML parsen:

```clojure
(require '[clj-yaml.core :as yaml])

;; Parsen eines YAML-Strings
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; Ausgabe:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### YAML aus Clojure generieren:

```clojure
(require '[clj-yaml.core :as yaml])

;; Konvertieren einer Clojure-Map in einen YAML-String
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; Ausgabe:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

Diese einfachen Operationen mit `clj-yaml` können in Clojure-Anwendungen integriert werden, um Konfigurationsdateien zu handhaben oder den Datenaustausch mit anderen Diensten oder Komponenten zu erleichtern, die YAML verwenden.
