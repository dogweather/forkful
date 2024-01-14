---
title:                "Python: Fehlersuchausgabe drucken"
simple_title:         "Fehlersuchausgabe drucken"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debugging ist ein wichtiger Teil des Programmierens und Debugging Output zu drucken ist eine nützliche Methode für Entwickler, um Fehler in ihrem Code zu finden und zu beheben. Durch das Ausdrucken von Debug Output können Sie den Zustand Ihres Programms zu bestimmten Zeitpunkten im Code überprüfen und sehen, ob die erwarteten Werte oder Variablenwerte richtig gesetzt werden. Dies kann dabei helfen, mögliche Fehlerquellen zu identifizieren und somit die Effizienz bei der Fehlersuche zu erhöhen.

## Wie

```python
# Beispiel einer einfachen Funktion zum Drucken von Debug Output
def square(x):
    """Berechnet das Quadrat einer Zahl und gibt den Schritt im Debug Output aus"""
    print("Debug Output: Schritt 1 - Funktionsaufruf square(", x, ")")
    result = x * x
    print("Debug Output: Schritt 2 - Berechnung des Quadrats von", x, ": ", result)
    return result

# Beispielaufruf der Funktion
print(square(5))
```

Der obige Code zeigt eine einfache Funktion, die das Quadrat einer Zahl berechnet und dabei den Debug Output verwendet. Durch das Drucken der Schritte innerhalb der Funktion können wir den Wert jeder Variablen überprüfen und sehen, wie sich dieser im Laufe des Programms ändert. Das kann besonders hilfreich sein, wenn die Funktion komplexer wird oder sich die Variablenwerte während der Ausführung ändern können.

## Deep Dive

Das Drucken von Debug Output ist eine effektive Methode, um den Ablauf des Codes zu verfolgen und potenzielle Fehler zu finden. Es kann jedoch auch zu einer Überflutung des Output-Fensters führen, wenn man alles ausdruckt. Deshalb ist es wichtig, die Ausgabe auf wichtige Informationen zu beschränken und gezielt zu wählen, welche Variablen oder Werte ausgegeben werden sollen. Auch das Formatieren der Ausgabe kann dabei helfen, den Debug Output übersichtlich und leichter lesbar zu gestalten.

Eine weitere Möglichkeit ist die Verwendung von Debuggern, die es ermöglichen, Schritt für Schritt durch den Code zu gehen und den aktuellen Wert jeder Variablen zu überprüfen. Dies kann besonders hilfreich sein, wenn es darum geht, komplexe Fehler zu finden oder das Verhalten einer Funktion zu untersuchen.

Insgesamt kann das Drucken von Debug Output eine wertvolle Ergänzung bei der Fehlersuche sein und sollte von Entwicklern in Betracht gezogen werden.

## Siehe auch

- [Python Debugger Tutorial auf Real Python](https://realpython.com/python-debugging-pdb/)
- [Debugging in Python with Print Statements auf Medium](https://medium.com/@aedigital/debugging-in-python-with-print-statements-75581f2690af)
- [Python Debugging With 'print' Statements auf YouTube](https://www.youtube.com/watch?v=gKVHnBl-Wr0)