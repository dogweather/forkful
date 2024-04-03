---
date: 2024-01-20 17:53:22.791929-07:00
description: 'How to: Du willst wissen, wie dein Code tickt? `print()` ist dein Freund.
  Platziere es neben Variablen und sieh dir die Magie an.'
lastmod: '2024-03-13T22:44:53.381262-06:00'
model: gpt-4-1106-preview
summary: Du willst wissen, wie dein Code tickt.
title: Debug-Ausgaben drucken
weight: 33
---

## How to:
Du willst wissen, wie dein Code tickt? `print()` ist dein Freund. Platziere es neben Variablen und sieh dir die Magie an:

```python
def addieren(x, y):
    ergebnis = x + y
    print("Das Ergebnis von", x, "+", y, "ist", ergebnis)
    return ergebnis

addieren(3, 4)
```

Beispielausgabe:
```
Das Ergebnis von 3 + 4 ist 7
```

Komplexer? Kein Problem. Schau dir `logging` an, das ist wie `print()`, aber mit Superkräften:

```python
import logging
logging.basicConfig(level=logging.DEBUG)
def dividieren(x, y):
    try:
        ergebnis = x / y
    except ZeroDivisionError:
        logging.error("Hoppla! Division durch Null.")
    else:
        logging.debug("Division erfolgreich: %s / %s = %s", x, y, ergebnis)
        return ergebnis

dividieren(10, 0)
dividieren(10, 2)
```

Beispielausgabe:
```
ERROR:root:Hoppla! Division durch Null.
DEBUG:root:Division erfolgreich: 10 / 2 = 5.0
```

## Deep Dive
Beim Debuggen gab's `print()` schon immer. Einfach, effektiv. Aber Code voller `print()`-Calls? Schwer zu managen. Deshalb haben kluge Leute `logging` eingeführt – kontrollierbar, flexibel, mächtig. Du kannst Log-Level setzen (INFO, WARNING, ERROR), Ausgaben formatieren und in Dateien schreiben. 

Alternativen? Klar! Debugger in Entwicklungsumgebungen, interaktive Tools wie `pdb` oder `ipdb`, und Tracking-Systeme wie Sentry geben dir noch mehr Einblick. Aber manchmal ist `print()` oder `logging` alles, was du brauchst.

Implementation? `print()` schickt Sachen auf `sys.stdout` (meist deine Konsole). `logging` macht mehr: Es bestimmt, wann, wo und wie deine Nachrichten landen. Mit `Handlers` bestimmst du Ziele: Konsole, Dateien, sogar über das Netzwerk.

## See Also
- Die offizielle Python Dokumentation zum `logging` Modul: https://docs.python.org/3/library/logging.html
- Der Python Debugger `pdb`: https://docs.python.org/3/library/pdb.html
- Eine Einführung in effektives Debuggen in Python: https://realpython.com/python-debugging-pdb/
- Sentry für fortgeschrittenes Error Tracking: https://sentry.io/welcome/
