---
title:                "Schreiben auf Standardfehler"
date:                  2024-01-19
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"

category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Schreiben auf `stderr` (Standardfehler) ermöglicht es Programmen, Fehlermeldungen separat von regulären Ausgaben zu senden. Dadurch können Entwickler und Benutzer Fehler leicht erkennen und die normale Ausgabe von der Fehlersuche trennen.

## How to:
Python bietet verschiedene Wege, um mit `stderr` zu arbeiten. Hier zwei einfache Methoden:

1. `sys.stderr.write()` nutzen:
```Python
import sys

sys.stderr.write('Das ist eine Fehlermeldung.\n')
```
Ausgabe im Terminal: `Das ist eine Fehlermeldung.`

2. `print()` mit dem Argument `file=sys.stderr` verwenden:
```Python
import sys

print('Das ist auch eine Fehlermeldung.', file=sys.stderr)
```
Ausgabe im Terminal: `Das ist auch eine Fehlermeldung.`

## Deep Dive
Historisch gesehen kommt das Konzept von `stderr` aus den Unix-Systemen, wo es zusammen mit `stdout` (Standardausgabe) und `stdin` (Standardeingabe) eingeführt wurde. Alternativen zu `sys.stderr.write()` und `print(file=sys.stderr)` sind zum Beispiel Logging-Frameworks, die mehr Funktionalität bieten. Intern funktioniert `sys.stderr` als ein File-Objekt, welches Methoden wie `.write()` und `.flush()` anbietet.

## See Also
- Python-Dokumentation für `sys`-Modul: https://docs.python.org/3/library/sys.html
- Python-Logging-HowTo: https://docs.python.org/3/howto/logging.html
- Erklärung zu Standard Streams: https://en.wikipedia.org/wiki/Standard_streams
