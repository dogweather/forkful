---
title:                "Debug-Ausgabe drucken"
html_title:           "Python: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Print-Ausgabe ist eine Methode, die Programmierer verwenden, um Fehler in ihrem Code zu identifizieren und zu beheben. Indem sie relevante Informationen während des Programmierprozesses drucken, können sie den Zustand ihres Codes überprüfen und mögliche Fehlerquellen finden.

## Wie geht's?

Um Debug-Informationen in Python zu drucken, verwenden Sie die Funktion `print()`. Sie können beliebige Variablen oder Ausdrücke in diese Funktion einfügen und sie werden dann während der Ausführung des Programms ausgegeben. Hier ist ein Beispiel:

```Python
nummer = 10
print(nummer)
```
**Ausgabe:**
```Python
10
```

Sie können auch mehrere Werte durch Kommas trennen, um sie in einer Zeile auszugeben. Hier ist ein Beispiel, bei dem wir die Ausgabe formatieren, um sie lesbarer zu machen:

```Python
name = "Hannah"
alter = 27
print("Mein Name ist", name, "und ich bin", alter, "Jahre alt.")
```
**Ausgabe:**
```Python
Mein Name ist Hannah und ich bin 27 Jahre alt.
```

Wenn Sie mehr Kontrolle über die Ausgabe haben möchten, können Sie die Formatierungsfunktion `format()` verwenden. Hier ist ein Beispiel, bei dem wir die Anzahl der Nachkommastellen einschränken:

```Python
pi = 3.141592653589793
print("Der Wert von Pi ist {:.2f}.".format(pi))
```
**Ausgabe:**
```Python
Der Wert von Pi ist 3.14.
```

## Tief eintauchen

Die Methode, Debugging-Informationen über die Ausgabe zu drucken, ist eine der einfachsten und am besten zugänglichen Möglichkeiten, um Fehler in Ihrem Code zu finden. In frühen Tagen der Programmierung war dies oft die einzige Möglichkeit, Fehler zu identifizieren und zu beheben. Heute gibt es jedoch andere Tools und Techniken, die ebenfalls für das Debugging verwendet werden können, wie z.B. integrierte Entwicklungsumgebungen (IDEs), Debugger und Logging-Bibliotheken.

Wenn Sie mehr über das Debugging in Python erfahren möchten, können Sie sich die Dokumentation von Python über Debugging ansehen oder sich mit spezifischeren Themen wie dem Verwenden von Breakpoints oder dem Einbinden von externen Bibliotheken für das Debugging beschäftigen.

## Siehe auch

- [Python-Debugging-Tutorial](https://realpython.com/python-debugging-tips/)
- [Offizielles Python-Debugging-Handbuch](https://docs.python.org/3/library/debug.html)
- [Python-Logging-Bibliothek](https://docs.python.org/3/library/logging.html)