---
title:                "Ein neues Projekt starten"
html_title:           "C#: Ein neues Projekt starten"
simple_title:         "Ein neues Projekt starten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein neues Projekt zu starten bedeutet, eine frische Programmierarbeit zu beginnen, oft mit Ziel, ein bestimmtes Problem zu lösen. Programmierer tun dies, um effizientere oder innovativere Lösungen zu entwickeln oder um ihre Fähigkeiten zu erweitern.

## So geht's:
In dieser Sektion sehen wir uns ein minimales Beispiel an, wie ein neues Projekt in Clojure mittels Leiningen aufgesetzt wird.

```clojure
;; Erstelle ein neues Projekt
lein new mein-projekt
```
Als Ausgabe sehen wir dann:

```
Generating a project called mein-projekt based on the 'default' template.
```
Jetzt können wir ins Verzeichnis wechseln und den Befehl `lein run` ausführen:

```clojure
cd mein-projekt
lein run
```
Das Ergebnis sagt uns, dass das Projekt erfolgreich erstellt wurde:

```
Hello, World!
```

## Vertiefung:
Im Jahr 2007 wurde Clojure erschaffen, um eine bessere Allzweck-Alternative zu Java zu bieten. Es gibt auch andere Alternativen wie Boot für Projektmanagement. Allerdings schätzen Nutzer an Leiningen die eher einfache Struktur der `project.clj`-Datei. Wenn man ein neues Projekt startet, legt Leiningen die nötige Verzeichnisstruktur an und erstellt eine anfangs minimale `project.clj`.

## Siehe auch:
- [Leiningen Tutorial](https://github.com/technomancy/leiningen/blob/stable/doc/TUTORIAL.md) 
- [Weitere Clojure Resourcen](https://clojure.org/community/resources)
- [Das Clojure Kochbuch](http://clojure-cookbook.com/)