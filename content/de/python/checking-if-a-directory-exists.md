---
title:    "Python: Überprüfen, ob ein Verzeichnis vorhanden ist"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Die Überprüfung, ob ein Verzeichnis existiert, ist eine wichtige Methode, um sicherzustellen, dass Ihr Python-Programm reibungslos funktioniert. Es ermöglicht Ihnen, zu überprüfen, ob ein bestimmtes Verzeichnis vorhanden ist, bevor Sie versuchen, darauf zuzugreifen, was Fehler vermeiden und die Effizienz Ihres Codes verbessern kann.

## Wie

Es gibt verschiedene Möglichkeiten, um in Python zu überprüfen, ob ein Verzeichnis existiert. Eine häufig verwendete Methode ist die Verwendung der `os` Bibliothek.

```Python
import os

if os.path.exists("Pfad/zum/Verzeichnis"):
  print("Das Verzeichnis existiert.")
else:
  print("Das Verzeichnis existiert nicht.")
```

Diese Methode nutzt die `os.path` Bibliothek, um festzustellen, ob ein Pfad existiert oder nicht. Es ist eine schnelle und einfache Möglichkeit, die Existenz eines Verzeichnisses zu überprüfen.

Ein weiterer Ansatz ist die Verwendung der `pathlib` Bibliothek, die in Python 3 eingeführt wurde.

```Python
from pathlib import Path

if Path("Pfad/zum/Verzeichnis").is_dir():
  print("Das Verzeichnis existiert.")
else:
  print("Das Verzeichnis existiert nicht.")
```

Dies ist eine modernere und objektorientierte Methode, die es Ihnen ermöglicht, verschiedene Dateieigenschaften einfach abzufragen.

## Deep Dive

Bei der Überprüfung, ob ein Verzeichnis existiert, gibt es einige wichtige Dinge zu beachten. Erstens müssen Sie sicherstellen, dass der angegebene Pfad korrekt ist, da sonst die Überprüfung nicht korrekt durchgeführt werden kann. Zu beachten ist auch, dass das Verzeichnis nicht nur existiert, sondern auch schreibbar ist, wenn Sie beabsichtigen, darauf zuzugreifen.

Wenn Sie ein bestimmtes Verzeichnis erstellen oder löschen möchten, ist es wichtig, sicherzustellen, dass das übergeordnete Verzeichnis vorhanden ist. Andernfalls können Fehler auftreten.

In Zukunft könnte es auch hilfreich sein, sich mit weiteren Funktionen aus der `os` und `pathlib` Bibliotheken vertraut zu machen, um die Dateiverwaltung noch effektiver zu gestalten.

## Siehe auch

- [Offizielle Dokumentation: os.path](https://docs.python.org/3/library/os.path.html)
- [Offizielle Dokumentation: pathlib](https://docs.python.org/3/library/pathlib.html)
- [Stack Overflow: Check if file exists](https://stackoverflow.com/questions/8933237/check-if-file-exists-python)