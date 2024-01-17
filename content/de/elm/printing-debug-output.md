---
title:                "Ausgabe von Fehlersuche"
html_title:           "Elm: Ausgabe von Fehlersuche"
simple_title:         "Ausgabe von Fehlersuche"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

Was ist das und warum machen Programmierer es?

Printing Debug Output ist eine Möglichkeit für Programmierer, Daten und Prozesse in ihren Code zu integrieren, um Probleme oder Fehler zu identifizieren und zu debuggen. Es ist eine effektive Methode, um den Ablauf des Codes zu überwachen und zu verstehen, was genau passiert. Das erleichtert die Fehlerbehebung und spart Zeit und Mühe bei der Entwicklung.

Wie geht's?

Debug Printing in Elm ist einfach, da die Sprache selbst Debugging-Tools wie den "Debug.log" -Befehl bietet. Durch Einbeziehung dieser Befehle in den Code können Entwickler verschiedene Daten wie Variablen, Musterabgleiche und Funktionseingaben in der Konsole ausgeben lassen. Hier ist ein Beispiel:

```
Elm.Debug.log "Gesamtkosten" (berechneGesamtkosten rechnung)
```

Dieser Befehl gibt den aktuellen Wert der Funktion "berechneGesamtkosten" in der Konsole aus, was für die Überprüfung der Ergebnisse und die Fehlerbehebung sehr nützlich sein kann.

Tiefer Einblick

Zurück zu den Anfängen der Softwareentwicklung mussten Programmierer verschiedene manuelle Methoden verwenden, um Bugs in ihrem Code zu finden und zu beheben. Mit dem Aufkommen von Debugging-Tools wie Print-Statements wurde dieser Prozess automatisiert und vereinfacht. Alternativen wie Debugging-Symbole und Tools zur Code-Analyse sind ebenfalls verfügbar, aber Print-Statements sind immer noch eine beliebte Wahl aufgrund ihrer Einfachheit und Wirksamkeit.

In Elm gibt es zwei Arten von Debug-Befehlen: "Debug.log" und "Debug.todo". Der letztere wird verwendet, um bestimmte Abschnitte des Codes zu markieren, die noch nicht implementiert sind, während ersterer zum Ausdrucken von Werten verwendet wird. Ein weiteres nützliches Feature sind die Debugging-Funktionen in der Chrome-Entwicklerkonsole, die speziell für Elm-Code entwickelt wurden.

Sieh dir auch an

- Elm-Dokumentation zum Debugging: https://guide.elm-lang.org/debugging/
- Stack Overflow Diskussion über die Wirksamkeit von Debug-Statements: https://stackoverflow.com/questions/569272/how-can-i-debug-my-javascript-code/569329#569329