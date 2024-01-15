---
title:                "Arbeiten mit YAML"
html_title:           "Clojure: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

# Warum

Warum sollte man sich mit YAML auseinandersetzen? Nun, YAML ist eine einfache, menschenlesbare Struktursprache, die hauptsächlich zur Konfiguration von Anwendungen, zum Speichern von Daten oder zum Austausch von Informationen zwischen verschiedenen Systemen verwendet wird.

## Wie

Um mit YAML in Clojure zu arbeiten, müssen wir zunächst die Bibliothek "clj-yaml" einbinden. Dies kann ganz einfach durch das Hinzufügen von `[org.clojure/data.yaml "1.0.0"]` als Abhängigkeit in unser Projekt.clj-Datei erreicht werden.

Im Folgenden sind ein paar Beispiele, wie wir YAML mit Clojure verwenden können:

```Clojure
(require '[clojure.data.yaml :as yaml])

;; Laden einer YAML-Datei als Clojure-Datenstruktur
(def config (yaml/read-string "foo: bar"))

;; Konvertieren einer Clojure-Datenstruktur in YAML
(yaml/generate-string {:name "Max" :age 30})

;; Erstellen einer YAML-Datei
(with-open [out-file (clojure.java.io/writer "config.yml")]
  (yaml/spit out-file {:language "Clojure" :version "1.10.1"}))

;; Einlesen von YAML-Dateien mit Fehlerbehandlung
(try
  (yaml/read-string "{ 123: abc")
  (catch Exception e
    (println "Fehler beim Einlesen von YAML: " (.getMessage e))))
```

Die Ausgabe für das letzte Beispiel sieht folgendermaßen aus:

```
Fehler beim Einlesen von YAML:  java.lang.NumberFormatException: For input string: "abc"
```

## Tiefergehende Einblicke

Clojure bietet uns viele Funktionen, um mit YAML-Daten umzugehen. Hier sind einige nützliche Funktionen, die wir verwenden können:

- `yaml/read` - Liest eine YAML-Datei und gibt eine Clojure-Datenstruktur zurück.
- `yaml/parse-stream` - Liest einen YAML-Stream und gibt eine Clojure-Datenstruktur zurück.
- `yaml/emit` - Gibt eine YAML-Zeichenkette aus einer Clojure-Datenstruktur zurück.
- `yaml/generate-stream` - Schreibt einen YAML-Stream aus einer Clojure-Datenstruktur.

Zusätzlich gibt es auch einige Optionen, die wir beim Lesen und Schreiben von YAML angeben können, z.B. die Unterstützung von interaktiven Datentypen wie `sets` und `maps`.

Insgesamt ist YAML eine einfache und effektive Möglichkeit, strukturierte Daten in unserer Clojure-Anwendung zu verwenden. Es ist jedoch wichtig, sorgfältig zu prüfen, ob YAML für unser spezifisches Anwendungsfall geeignet ist, da es nicht so stark typisiert ist wie andere Formate wie z.B. JSON.

## Siehe auch

- [Offizielle YAML-Website](https://yaml.org/)
- [Dokumentation zu clj-yaml](https://github.com/clj-commons/clj-yaml)