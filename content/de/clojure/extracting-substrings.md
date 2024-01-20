---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Extrahieren von Teilstrings (substrings) bezieht sich auf die Aufnahme kleinerer Zeichenketten aus einer größeren Zeichenkette. Programmierer tun dies, um relevante Informationen in umfangreichen Daten oder Text zu finden und zu verwenden.

## Wie mache ich das?

Clojure bietet eine Vielzahl von Funktionen zur substraktion von Strings. Eine einfache besteht darin, die "subs"-Funktion zu verwenden. Hier ist ein Beispiel:

```Clojure
(def str "Hallo, Welt!") 
(subs str 0 5)
```

Die Ausgabe wird "Hallo" sein. Die Funktion "subs" erhält den Anfangs- und Endindex des Substrings, den Sie abschneiden möchten.

## Vertiefung

Geschichtlich betrachtet ist das Extrahieren von Teilstrings eine grundlegende Operation in vielen Programmiersprachen und hat seinen Ursprung in den frühesten Tagen der Informatik. In Clojure, das auf der Java Virtual Machine (JVM) basiert, profitiert es von den vorhandenen String-Manipulationsmethoden in Java. 

Auf der anderen Seite haben Sie alternative Methoden, um Substrings in Clojure zu extrahieren. Sie können reguläre Ausdrucke (Regex) oder Funktionen wie "split-at" und "join" verwenden, um ähnliche Ergebnisse zu erzielen.

Was die Implementierung betrifft, verwendet Clojure bei der Verwendung der "subs"-Funktion den Java-Substring. Dies hat den Vorteil, dass keine neuen Zeichen erstellt werden, sondern nur Zeiger auf die bestehende Zeichenkette manipuliert werden, was die Operation sehr effizient macht.

## Siehe auch

Um Ihr Wissen und Ihre Fähigkeiten im Umgang mit Strings in Clojure weiter zu vertiefen, sehen Sie sich bitte die folgenden Ressourcen an:

2. [Clojure: String Functions](https://clojuredocs.org/clojure.string)