---
title:                "Clojure: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

Wenn du ein Entwickler bist, der mit Daten arbeitet, hast du wahrscheinlich schon von YAML gehört. YAML, auch bekannt als "YAML Ain't Markup Language", ist eine einfache und benutzerfreundliche Art, Daten in strukturierter Form zu speichern. Es ist eine große Hilfe, wenn es darum geht, Daten zwischen verschiedenen Programmen oder Plattformen auszutauschen.

## Wie man YAML in Clojure verwendet

Das Einbinden von YAML in deine Clojure-Projekte ist ganz einfach. Zunächst musst du die Bibliothek [clj-yaml](https://github.com/lancepantz/clj-yaml) zu deinem `project.clj` hinzufügen. Dann kannst du mit `use` oder `require` den Namespace `clj-yaml.core` importieren.

Lass uns ein einfaches Beispiel betrachten, bei dem wir YAML-Daten aus einer Datei auslesen und in ein Clojure-Map umwandeln. Hier ist der Code für unsere Datei "example.yml":

```YAML
name: Max
age: 25
favorite_color: Blue
```

Und hier ist der Code, den wir in unserem Clojure-Projekt verwenden würden:

```Clojure
(use 'clj-yaml.core)

(def data (load-data "example.yml"))
(println (:name data)) ;; Ausgabe: Max
(println (:age data)) ;; Ausgabe: 25
(println (:favorite_color data)) ;; Ausgabe: Blue
```

Wie du sehen kannst, können wir mit der `load-data` Funktion die YAML-Daten in eine Clojure-Map konvertieren und dann ganz einfach auf die einzelnen Werte zugreifen.

## Tiefergehende Erläuterungen

Du hast wahrscheinlich bemerkt, dass wir bei der Verwendung von `load-data` keine explizite Typangabe gemacht haben. Das liegt daran, dass YAML flexible Datentypen unterstützt und daher eine explizite Typenumwandlung in Clojure nicht notwendig ist.

Außerdem können wir mit YAML auch komplexe Strukturen wie Listen und verschachtelte Maps speichern. Hier ist ein Beispiel für eine YAML-Datei mit einer Liste von Personen:

```YAML
- name: Anna
  age: 30
  favorite_color: Red
- name: Ben
  age: 28
  favorite_color: Green
- name: Clara
  age: 32
  favorite_color: Yellow
```

Und hier ist der Code, den wir verwenden würden, um die Daten in eine Clojure-Liste von Maps zu konvertieren:

```Clojure
(use 'clj-yaml.core)

(def data (load-data "example.yml"))
(doseq [person data]
  (println (:name person))) ;; Ausgabe: Anna Ben Clara
```

Dies ist nur ein kleiner Einblick in die Verwendung von YAML in Clojure. Es gibt viele weitere Funktionen und Möglichkeiten, die du erkunden kannst. Wir empfehlen dir, die offizielle Dokumentation von [YAML](https://yaml.org/) und [clj-yaml](https://github.com/lancepantz/clj-yaml) zu lesen, um mehr darüber zu erfahren.

## Siehe auch

- [Offizielle YAML Dokumentation](https://yaml.org/)
- [clj-yaml Bibliothek](https://github.com/lancepantz/clj-yaml)