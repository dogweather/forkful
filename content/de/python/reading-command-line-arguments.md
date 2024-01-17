---
title:                "Das Lesen von Befehlszeilenargumenten."
html_title:           "Python: Das Lesen von Befehlszeilenargumenten."
simple_title:         "Das Lesen von Befehlszeilenargumenten."
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen von Befehlszeilenargumenten ist eine wichtige Fähigkeit für Programmierer. Es ermöglicht uns, Eingaben vom Benutzer zu erhalten und diese in unserer Python-Anwendung zu verarbeiten. Dadurch können wir unsere Programme flexibler gestalten und auf verschiedene Situationen reagieren.

## So geht's:

Die folgenden Beispiele zeigen, wie man Befehlszeilenargumente in Python lesen kann:

```Python
import sys
# liest das erste Argument, welches nach dem Dateinamen eingegeben wird
arg1 = sys.argv[1] 
print("Das erste Argument ist:", arg1)

# liest eine Liste aller Argumente
args = sys.argv[1:] 
print("Alle Argumente:", args)
```

Beispielaufruf in der Kommandozeile: `python my_program.py argument1 argument2`

Ausgabe:

```
Das erste Argument ist: argument1
Alle Argumente: ['argument1', 'argument2']
```

## Tiefere Einblicke:

Das Lesen von Befehlszeilenargumenten ist ein grundlegender Bestandteil der Unix-Philosophie "Kleine Werkzeuge schnell miteinander kombinieren". Es ermöglicht eine einfache Kommunikation zwischen verschiedenen Programmen und stellt sicher, dass Programme unabhängig voneinander funktionieren können.

Alternativ zum Lesen von Befehlszeilenargumenten können auch Umgebungsvariablen oder Konfigurationsdateien genutzt werden, um Eingaben vom Benutzer zu erhalten. Diese Methoden bieten eine größere Flexibilität, erfordern jedoch auch eine höhere Komplexität bei der Implementierung.

Um Befehlszeilenargumente in Python zu lesen, wird die `sys` Bibliothek genutzt, die Funktionen und Variablen für die Interaktion mit dem Python-Interpreter bereitstellt.

## Siehe auch:

- [Python-Dokumentation zu sys.argv](https://docs.python.org/3/library/sys.html#sys.argv)
- [Unix Philosophy](https://en.wikipedia.org/wiki/Unix_philosophy)