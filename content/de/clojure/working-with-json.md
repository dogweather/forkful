---
title:                "Clojure: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

In der heutigen digitalen Welt sind Daten von großer Bedeutung. Eines der wichtigsten Formate zur Speicherung und Übertragung von Daten ist JSON. Als Clojure-Entwickler ist es daher von großer Bedeutung zu wissen, wie man mit JSON umgeht.

## Wie man JSON in Clojure verwendet

Die Verwendung von JSON in Clojure ist relativ einfach. Zunächst müssen wir jedoch die Clojure-Bibliothek ["json"](https://github.com/clojure/data.json) importieren. Dies kann durch Hinzufügen der folgenden Zeile zu unserer `project.clj` Datei erreicht werden:

```clojure
[org.clojure/data.json "0.2.6"]
```

Sobald die Bibliothek importiert wurde, können wir JSON-Daten mit Hilfe der Funktionen `json/read-str` und `json/write-str` einlesen und ausgeben. Schauen wir uns hierzu ein Beispiel an:

```clojure
(ns example.core
  (:require [clojure.data.json :as json]))

;; JSON-String einlesen
(def json-daten (json/read-str "{\"name\": \"Max Mustermann\", \"email\": \"max.mustermann@example.com\"}"))

;; JSON-Daten ausgeben
(println "Name: " (:name json-daten))
(println "Email: " (:email json-daten))

;; JSON-Objekt erstellen und als String ausgeben
(def json-objekt {"name" "Max Mustermann"
                  "email" "max.mustermann@example.com"})

(println "JSON-String: " (json/write-str json-objekt))
```

Das obige Beispiel liest einen JSON-String ein, extrahiert die Werte für den Namen und die E-Mail-Adresse und gibt sie aus. Anschließend wird ein JSON-Objekt erstellt und als String ausgegeben.

## Tiefergehende Informationen

Neben den oben genannten Basiskonzepten gibt es noch eine Vielzahl weiterer Funktionen in der Clojure-Bibliothek "json". Hierzu gehören unter anderem die Möglichkeit, JSON-Dateien direkt einzulesen und auszugeben, das Arbeiten mit verschachtelten Datenstrukturen und die Konvertierung von Clojure-Daten in JSON-Strings und umgekehrt.

Ein weiteres wichtiges Konzept beim Umgang mit JSON ist die JSON Schema. Hierbei handelt es sich um eine Beschreibung der Struktur und des Formats von JSON-Daten. Mit Hilfe von JSON Schema können wir sicherstellen, dass die eingelesenen Daten den erwarteten Anforderungen entsprechen. Eine gute Einführung in JSON Schema findest du [hier](https://json-schema.org/learn/getting-started-step-by-step.html).

## Siehe auch

- [Clojure data.json Dokumentation](https://github.com/clojure/data.json)
- [Offizielle JSON Webseite](https://www.json.org/)
- [JSON Schema Dokumentation](https://json-schema.org/)