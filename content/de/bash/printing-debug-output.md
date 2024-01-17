---
title:                "Debug-Ausgabe drucken"
html_title:           "Bash: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Debug-Ausgaben sind kurze und nützliche Textnachrichten, die beim Programmieren verwendet werden, um zu überprüfen, ob der Code wie erwartet funktioniert. Programmierer nutzen sie, um Fehler zu finden und zu beheben, sowie um die Funktionsweise des Codes besser zu verstehen.

## Wie geht's?
Um Debug-Ausgaben in Bash zu erstellen, kannst du das integrierte Kommando ```echo``` verwenden. Hier ist ein Beispiel:

```bash
echo "Die Debug-Ausgabe wird hier angezeigt."
```

Die Ausgabe sieht dann so aus:

```
Die Debug-Ausgabe wird hier angezeigt.
```

Du kannst auch Variablen in die Ausgabe einfügen, um bestimmte Werte oder Informationen zu überprüfen. Hier ist ein Beispiel:

```bash
var1="Hallo!"
var2=42
echo "Das ist Variable 1: $var1 und das ist Variable 2: $var2"
```

Die Ausgabe sieht dann so aus:

```
Das ist Variable 1: Hallo! und das ist Variable 2: 42
```

## Tiefer Einblick
Debug-Ausgaben gibt es schon seit den frühen Tagen der Programmierung. Früher wurden sie vor allem in textbasierten Programmiersprachen wie C verwendet, um zu überprüfen, ob der Code richtig ausgeführt wird. Heutzutage werden sie immer noch verwendet, aber auch moderne Programmiersprachen wie Bash haben integrierte Debugging-Tools, um dieses Verfahren zu erleichtern.

Eine Alternative zu Debug-Ausgaben sind sogenannte Debugger-Tools, die es ermöglichen, den Code in Echtzeit zu überwachen und zu untersuchen. Diese können jedoch komplexer und schwieriger zu benutzen sein, vor allem für Anfänger.

Die Implementierung von Debug-Ausgaben in Bash ist relativ einfach und erfordert keine speziellen Tools oder Kenntnisse. Es ist eine schnelle und effektive Methode, um den Code während der Entwicklung zu überprüfen.

## Siehe auch
- [Bash Debugging Basics](https://devhints.io/bash-debugging)
- [Bash Scripting Best Practices – Debugging](https://betterprogramming.pub/bash-scripting-best-practices-debugging-1555154ce746)
- [Debugging Shell Scripts](https://www.gnu.org/software/bash/manual/html_node/Debugging-Shell-Scripts.html) (offizielle Bash-Dokumentation)