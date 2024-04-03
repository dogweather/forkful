---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:00.506338-07:00
description: "Wie geht das: Clojure enth\xE4lt keine eingebauten Funktionen zum Arbeiten\
  \ mit JSON, daher verwenden Sie typischerweise Bibliotheken von Drittanbietern.\u2026"
lastmod: '2024-03-13T22:44:53.441609-06:00'
model: gpt-4-0125-preview
summary: "Clojure enth\xE4lt keine eingebauten Funktionen zum Arbeiten mit JSON, daher\
  \ verwenden Sie typischerweise Bibliotheken von Drittanbietern."
title: Arbeiten mit JSON
weight: 38
---

## Wie geht das:
Clojure enthält keine eingebauten Funktionen zum Arbeiten mit JSON, daher verwenden Sie typischerweise Bibliotheken von Drittanbietern. `cheshire` und `jsonista` sind aufgrund ihrer Benutzerfreundlichkeit und Leistung beliebte Auswahlmöglichkeiten.

### Cheshire verwenden
Zuerst fügen Sie Cheshire zu Ihren Projekt-Abhängigkeiten in `project.clj` hinzu:
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

Um einen JSON-String in eine Clojure-Map zu parsen und eine Map in einen JSON-String zu konvertieren:

```clj
(require '[cheshire.core :as json])

;; JSON-String zu Clojure-Map parsen
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; Clojure-Map in JSON-String konvertieren
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### Jsonista verwenden
Fügen Sie Jsonista Ihrem Projekt `project.clj` hinzu:
```clj
[jsonista "0.3.2"]
```

Ähnliche Operationen mit Jsonista:

```clj
(require '[jsonista.core :as j])

;; JSON-String zu Clojure parsen
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; Clojure-Map in JSON-String konvertieren
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

In beiden Bibliotheken haben Sie die Möglichkeit, komplexere Datenstrukturen zu kodieren und zu dekodieren, und es gibt zusätzliche Funktionen und Parameter, die eine Anpassung der Serialisierungs- und Deserialisierungsprozesse ermöglichen. Für die meisten Anwendungen bietet die demonstrierte Funktionalität eine solide Grundlage für die Arbeit mit JSON in Clojure-Anwendungen.
