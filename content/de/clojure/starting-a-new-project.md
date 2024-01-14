---
title:                "Clojure: Ein neues Projekt beginnen"
programming_language: "Clojure"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

German Translation:

## Warum

Es gibt viele Gründe, warum es sich lohnt, ein neues Programmierprojekt in Clojure zu starten. Zum einen bietet Clojure eine elegante Syntax, die es ermöglicht, komplexe Probleme auf einfache und elegante Weise zu lösen. Außerdem ist Clojure eine funktionale Programmiersprache, die auf der JVM läuft, was bedeutet, dass sie sowohl die Vorteile von Funktionen als auch die Leistung von Java bietet. Kurz gesagt, Clojure ist die perfekte Wahl für diejenigen, die leistungsstarke und moderne Projekte erstellen möchten.

## Wie man startet

1. Installieren Sie Java Development Kit (JDK) und Clojure auf Ihrem Computer.
2. Öffnen Sie Ihr Terminal und starten Sie den Clojure REPL mit dem Befehl `clj`.
3. Erstellen Sie ein neues Clojure-Projekt mit dem Befehl `(defproject mein-projekt "1.0.0" :dependencies [[org.clojure/clojure "1.10.1"]])`
4. Navigieren Sie in den erstellten Projektordner mit dem Befehl `cd mein-projekt`.
5. Öffnen Sie die Datei `src/mein-projekt/core.clj` mit einem Code-Editor Ihrer Wahl.

```Clojure
(ns mein-projekt.core
  (:gen-class))

(defn -main [& args]
  (println "Hallo von meinem neuen Clojure-Projekt!"))
```

6. Speichern Sie die Datei und kehren Sie zurück zum Terminal.
7. Führen Sie das Projekt mit dem Befehl `clj -m mein-projekt.core` aus.
8. Sie sollten die Ausgabe "Hallo von meinem neuen Clojure-Projekt!" sehen.

## Tiefes Eintauchen

Nachdem Sie Ihr neues Clojure-Projekt erstellt haben, können Sie jetzt tiefer in die Sprache eintauchen. Clojure bietet eine Vielzahl von Bibliotheken und Werkzeugen, die Ihnen helfen können, Ihr Projekt zu entwickeln. Ein paar wichtige Themen, mit denen Sie sich beschäftigen sollten, sind:

- Funktionale Programmierung
- Unveränderlichkeit
- Threads und Concurrent Programming
- Persistenz und Datenstrukturen

Es gibt auch viele Ressourcen online, die Ihnen dabei helfen können, wie z.B. Bücher, Tutorials, Foren und Meetups. Stellen Sie sicher, dass Sie sich jederzeit über die neuesten Entwicklungen in der Clojure-Community auf dem Laufenden halten.

## Siehe auch

- [Clojure Dokumentation](https://clojure.org/)
- [Clojure Forum](https://clojureverse.org/)
- [Clojure Meetup-Gruppen](https://www.meetup.com/topics/clojure/)
- [Clojure Bücher und Tutorials](https://github.com/oremacs/awesome-clojure)