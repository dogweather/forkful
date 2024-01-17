---
title:                "Unterstrings extrahieren"
html_title:           "Clojure: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

Was & Warum?
Das Extrahieren von Teilzeichenketten ist eine gebräuchliche Aufgabe in der Programmierung, bei der ein Teil einer Zeichenkette ausgewählt und als eigenständige Zeichenkette gespeichert wird. Dies kann nützlich sein, um bestimmte Teilinformationen einer größeren Zeichenkette zu nutzen oder zu verarbeiten. Programmierer verwenden diese Technik, um Daten zu filtern oder zu manipulieren.

Wie geht's?
Die Programmiersprache Clojure bietet verschiedene Funktionen für das Extrahieren von Teilzeichenketten. Mit der Funktion ```subs``` können wir einen bestimmten Bereich einer Zeichenkette auswählen und als eigenständige Zeichenkette speichern. Zum Beispiel ```(subs "Hallo Welt" 0 5)``` würde die ersten 5 Zeichen ("Hallo") als neue Zeichenkette zurückgeben. Wir können auch negative Indizes verwenden, um die Extraktion von der rechten Seite der Zeichenkette zu starten, zum Beispiel ```(subs "Hallo Welt" -4)``` würde die letzten 4 Zeichen ("Welt") zurückgeben. Weitere Funktionen wie ```substring``` oder ```split-at``` bieten zusätzliche Möglichkeiten zum Extrahieren von Teilzeichenketten.

Tiefgehende Einblicke
Das Extrahieren von Teilzeichenketten ist eine grundlegende Operation der Zeichenkettenmanipulation und wird in vielen Programmiersprachen unterstützt. In Clojure werden Teilzeichenketten als eigenständige Zeichenketten behandelt und nicht als Verweise auf die ursprüngliche Zeichenkette. Andere Programmiersprachen wie Java oder Javascript bieten ähnliche Funktionen, aber mit unterschiedlicher Syntax.

Siehe auch
- Clojure-Dokumentation zu Teilzeichenketten: https://clojuredocs.org/clojure.core/subs
- Vergleich von Zeichenkettenextraktionsfunktionen in verschiedenen Programmiersprachen: https://www.programiz.com/java-programming/library/substring
- Übersicht über die grundlegenden Zeichenkettenoperationen in Clojure: https://clojure.org/reference/strings