---
title:                "Clojure: Ein neues Projekt beginnen"
simple_title:         "Ein neues Projekt beginnen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum

Das Programmieren mit Clojure ist eine großartige Möglichkeit, um leistungsstarke und skalierbare Anwendungen zu entwickeln. Egal, ob Sie ein erfahrener Entwickler sind oder gerade erst anfangen, Clojure bietet eine elegante Syntax und eine aktive Community, die Ihnen dabei hilft, Ihre Projekte zum Leben zu erwecken.

## Wie geht man vor

Um ein neues Projekt in Clojure zu starten, benötigen Sie zunächst die Clojure-Bibliothek und einen geeigneten Editor oder eine integrierte Entwicklungsumgebung (IDE). Die am häufigsten verwendete Bibliothek ist Leiningen, die Sie mit dem Befehl `lein new app` installieren können. Anschließend erstellen Sie eine `core.clj`-Datei und können mit dem Codieren beginnen.

Hier ist ein Beispiel, wie Sie Hallo-Welt in Clojure ausgeben können:

```Clojure
(ns hello-world.core
  (:gen-class))

(defn -main []
  (println "Hallo Welt!"))
```

Nachdem Sie Ihr Programm geschrieben haben, können Sie es mit dem Befehl `lein run` ausführen und die Ausgabe "Hallo Welt!" sehen. Das ist schon alles, was nötig ist, um ein einfaches Clojure-Programm zu schreiben!

## Tiefergehende Informationen

Wenn Sie tiefer in die Welt von Clojure eintauchen möchten, gibt es verschiedene Ressourcen, die Ihnen helfen können, den Einstieg zu erleichtern. Eine gute Anlaufstelle ist das offizielle Clojure-Handbuch, das eine umfassende Einführung in die Sprache bietet. Außerdem gibt es zahlreiche Tutorials und Beispiele auf Websites wie ClojureDocs und Clojure Gazette.

Eine weitere wichtige Ressource ist die Community von Clojure-Entwicklern, die Sie in Foren, Social Media-Gruppen oder auf Meetups finden können. Hier können Sie Fragen stellen, Probleme lösen und von anderen erfahrenen Entwicklern lernen.

## Siehe auch

- [Offizielles Clojure-Handbuch](https://clojure.org/guides/getting_started)
- [ClojureDocs](https://clojuredocs.org/)
- [Clojure Gazette](https://clojure-gazette.com/)