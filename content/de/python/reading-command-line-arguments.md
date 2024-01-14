---
title:    "Python: Lesen von Befehlszeilenargumenten"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist eine wichtige Fähigkeit für jeden Python-Programmierer. Es ermöglicht die Interaktion mit dem Benutzer und gibt die Möglichkeit, dynamische Parameter in das Programm einzubinden.

## Wie man es macht

Um Befehlszeilenargumente in Python zu lesen, müssen wir das `sys`-Modul importieren. Dann können wir die Argumente über die Liste `argv` abrufen.

```Python
import sys

# Lesen der Argumente
arguments = sys.argv

# Ausgabe des ersten Arguments (index 0 ist der Dateiname)
print("Das erste Argument ist: " + arguments[1])
```

Die Ausgabe sieht dann folgendermaßen aus:

```
> python program.py hallo
Das erste Argument ist: hallo
```

## Tiefergehende Analyse

Es gibt einige mögliche Optionen, wie wir die Argumente weiterverarbeiten können. Zum Beispiel können wir die `len()`-Funktion verwenden, um die Anzahl der Argumente zu bestimmen. Oder wir können die Methode `argv.pop(0)` verwenden, um das erste Argument (also den Dateinamen) auszuschließen.

```Python
import sys

# Ausschließen des Dateinamens
sys.argv.pop(0)

# Ausgabe aller Argumente
print("Alle Argumente: " + str(sys.argv))

# Ausgabe der Anzahl der Argumente
print("Anzahl der Argumente: " + str(len(sys.argv)))

```

Die Ausgabe sieht dann beispielsweise so aus:

```
> python program.py hello world
Alle Argumente: ['hello', 'world']
Anzahl der Argumente: 2
```

## Siehe auch

- [Python-Dokumentation zu `sys.argv`](https://docs.python.org/3/library/sys.html#sys.argv)
- [Tutorial zu Befehlszeilenargumenten in Python](https://www.geeksforgeeks.org/command-line-arguments-in-python/)
- [Beispiele für die Verwendung von `argv` in Python](https://www.tutorialspoint.com/python/python_command_line_arguments.htm)