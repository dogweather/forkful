---
title:                "Ein neues Projekt beginnen"
html_title:           "Clojure: Ein neues Projekt beginnen"
simple_title:         "Ein neues Projekt beginnen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

Hallo liebe Leser:innen,

in diesem Artikel werden wir uns mit dem Erstellen eines neuen Projekts in Clojure beschäftigen. Aber bevor wir in die Details eintauchen, lass uns kurz betrachten, was genau ein neues Projekt ist und warum Programmiere:innen immer wieder neue Projekte starten.

## Was & Warum?

Ein neues Projekt in Clojure zu erstellen bedeutet, eine neue Code-Basis aufzubauen und mit der Programmierung eines neuen Programms oder einer Anwendung zu beginnen. Warum tun wir das? Weil es uns ermöglicht, neue Ideen und Funktionen zu verwirklichen, unsere Fähigkeiten zu verbessern und auch Fehler oder Probleme in unseren alten Projekten zu beheben. Es ist eine aufregende Herausforderung, die uns neue Möglichkeiten eröffnet.

## How to:

Um ein neues Projekt in Clojure zu erstellen, gibt es ein paar Schritte, die du befolgen kannst:

1. Installiere die neueste Version von Clojure auf deinem Computer.
2. Öffne eine Lein-Projekt-Datei und gib die spezifischen Details für dein neues Projekt ein, wie z.B. Name, Version und Abhängigkeiten.
3. Öffne die Terminal oder die Befehlszeile und navigiere zum Verzeichnis deines neuen Projekts.
4. Führe den Befehl ```lein repl``` aus, um die Clojure REPL zu öffnen.
5. Nun kannst du deine ersten Code-Zeilen schreiben und dein neues Projekt zum Laufen bringen.

Hier ist ein Beispiel für eine Lein-Projekt-Datei:

```Clojure
(defproject my-project "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :plugins [[lein-exec "0.3.7"]]
  :exec-fn -main)
```

Und hier ist ein Beispiel für eine einfache Funktion, die die Summe von zwei Zahlen berechnet:

```Clojure
(defn sum [a b]
  (+ a b))

(sum 2 3) ; Output: 5
```

## Deep Dive:

Clojure ist eine funktionale Programmiersprache, die auf der JVM (Java Virtual Machine) läuft. Sie wurde im Jahr 2007 von Rich Hickey entwickelt und ist stark von anderen funktionalen Programmiersprachen wie Lisp und Scheme inspiriert. Das bedeutet, dass wir in Clojure häufiger mit Funktionen als mit Objekten arbeiten.

Es gibt auch andere Möglichkeiten, ein neues Projekt in Clojure zu erstellen, z.B. mit dem Build-Tool Boot oder mit einem integrierten Entwicklungsumfeld wie Cursive oder VSCode. Es ist jedoch immer noch empfehlenswert, Leiningen (kurz "lein") zu nutzen, da es das am weitesten verbreitete und unterstützte Tool ist.

Während des Erstellungsprozesses wird Leiningen automatisch alle benötigten Abhängigkeiten herunterladen und verwalten, so dass wir uns darauf konzentrieren können, Code zu schreiben.

## See Also:

- Clojure Dokumentation: https://clojure.org
- Leiningen Dokumentation: https://leiningen.org
- Boot Dokumentation: https://boot-clj.com