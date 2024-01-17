---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Clojure: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Was & Warum?

Das Lesen von Kommandozeilenargumenten ist eine Methode, mit der Programmierer Informationen aus der Eingabeaufforderung erhalten. Dies ermöglicht es ihnen, interaktive Programme zu erstellen und Benutzereingaben zu berücksichtigen.

Wie geht's?

Um Kommandozeilenargumente in Clojure zu lesen, können Sie den Befehl `*command-line-args*` verwenden. Im Folgenden finden Sie ein Beispiel, wie Sie die Argumente ausgeben können:

```Clojure
(def args *command-line-args*)
(prn "Eingegebene Argumente:" args)
```

Ausgabe:

```
Eingegebene Argumente: [arg1 arg2 arg3]
```

Der Befehl `*command-line-args*` gibt eine Liste von Zeichenketten zurück, die die eingegebenen Argumente darstellen. Diese können dann in Ihrem Programm verwendet werden, um Entscheidungen zu treffen oder Berechnungen durchzuführen.

Tiefer tauchen

Das Lesen von Kommandozeilenargumenten ist eine gängige Praxis in der Programmierung und wird seit langem verwendet. Es gibt auch alternative Methoden, wie z.B. das Erstellen von interaktiven Menüs oder die Verwendung von Einstellungen in einer Konfigurationsdatei.

In Clojure wird die `clojure.main`-Funktion verwendet, um Kommandozeilenargumente zu lesen und an das eigentliche Programm weiterzugeben. Diese Funktion ist Teil der Kernbibliothek von Clojure und daher immer verfügbar.

Siehe auch

Für weitere Informationen über `*command-line-args*` und andere nützliche Funktionen in Clojure, können Sie die offizielle Dokumentation unter https://clojure.org/api/cheatsheet und https://clojure.github.io/clojure/clojure.main-api.html besuchen.