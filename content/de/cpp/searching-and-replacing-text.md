---
title:                "C++: Suchen und Ersetzen von Text"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Das Ersetzen von Text in einem Programm kann aus verschiedenen Gründen wichtig sein. Zum Beispiel kann es helfen, wiederkehrende Fehler oder Textpassagen schnell und effizient zu korrigieren. Auch bei der Umwandlung von Daten oder beim Formatieren von Text kann das Suchen und Ersetzen nützlich sein.

## Wie man es macht

Das Suchen und Ersetzen von Text in C++ ist relativ einfach. Zunächst importieren wir die Bibliothek "string", die uns Funktionen zum Manipulieren von Zeichenketten zur Verfügung stellt:

```C++
#include <string>
```

Nun erstellen wir eine Zeichenkette und weisen ihr einen Wert zu:

```C++
std::string text = "Hallo, wie geht es dir?";
```

Um nun bestimmte Wörter oder Zeichenfolgen in dieser Zeichenkette zu suchen und zu ersetzen, können wir die Funktion "replace" verwenden. Diese erhält drei Parameter: die Position, ab der die Ersetzung erfolgen soll, die Anzahl der zu ersetzenden Zeichen und die Zeichenkette, mit der ersetzt werden soll. Wir können auch angeben, dass alle Vorkommen eines bestimmten Worts oder einer Zeichenfolge ersetzt werden sollen, indem wir den zusätzlichen Parameter "npos" verwenden. Hier ist ein Beispiel:

```C++
std::string text2 = "Ich mag Bananen sehr.";
text2.replace(8, 7, "Äpfel"); // Ersetzt "Bananen" durch "Äpfel"
```

Die Ausgabe dieser Zeichenkette wäre dann: "Ich mag Äpfel sehr."

## Tiefere Einblicke

Die Funktion "replace" ist sehr nützlich, wenn es darum geht, einfache Textmodifikationen durchzuführen. Jedoch gibt es in C++ auch weitere Funktionen und Bibliotheken, die noch leistungsfähigere Such- und Ersetzungsfunktionen bieten. Zum Beispiel kann die Bibliothek "boost/algorithm/string" bei der Arbeit mit größeren Textmengen und komplexeren Suchmustern hilfreich sein.

Es ist auch wichtig zu beachten, dass die Funktion "replace" nicht immer die beste Wahl ist, wenn es darum geht, Text in einer Datei oder einer Datenbank zu ersetzen. In diesen Fällen sollten spezielle Funktionen und Anwendungen wie "sed" oder "awk" verwendet werden, um Text effizienter zu ersetzen.

## Siehe auch

- [C++ String Replace Dokumentation] (https://www.cplusplus.com/reference/string/string/replace/)
- [Boost String Algorithm Bibliothek] (https://www.boost.org/doc/libs/1_37_0/doc/html/string_algo.html)