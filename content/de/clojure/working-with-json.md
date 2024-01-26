---
title:                "Arbeiten mit JSON"
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON (JavaScript Object Notation) ist ein leichtgewichtiges Format zum Datenaustausch. In Clojure nutzen wir es, weil es menschenlesbar und maschinenverarbeitbar ist – perfekt für Web-APIs und Config-Files.

## How to:
Um JSON in Clojure zu bearbeiten, nutzen wir die `cheshire` Bibliothek. Hier wie's geht:

```Clojure
;; Dependencies hinzufügen 
;; `[cheshire "5.10.1"]` zu project.clj oder deps.edn

(require '[cheshire.core :as json])

;; JSON-String parsen
(def json-str "{\"name\":\"Ada\",\"age\":30}")
(def data (json/parse-string json-str))
;; => {"name" "Ada", "age" 30}

;; Clojure Map in JSON-String konvertieren
(def clj-map {:name "Ada", :age 30})
(def json-output (json/generate-string clj-map))
;; => "{\"name\":\"Ada\",\"age\":30}"
```

## Deep Dive:
JSON gibt es seit den frühen 2000ern, populär gemacht durch JavaScripts Web-Dominanz. Alternativen wie XML sind oft komplexer, mit Clojure bieten sich jedoch auch EDN oder Transit als weitere Formate an. `cheshire` nutzt intern Jackson, eine schnelle JSON-Bibliothek für Java, was die Verarbeitung effizient macht.

## See Also:
- Cheshire GitHub Repo: [https://github.com/dakrone/cheshire](https://github.com/dakrone/cheshire)
- EDN, eine Clojure-eigene Datennotation: [https://github.com/edn-format/edn](https://github.com/edn-format/edn)
- Transit, für effizienteren Datenaustausch: [https://github.com/cognitect/transit-clj](https://github.com/cognitect/transit-clj)
