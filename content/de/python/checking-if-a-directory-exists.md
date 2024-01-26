---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:57:57.841939-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Überprüfen, ob ein Verzeichnis existiert, bedeutet, im Dateisystem nachzuschauen, ob ein bestimmter Ordner da ist. Programmierer tun dies, um Fehler zu vermeiden, die auftreten, wenn man versucht, mit einem nicht vorhandenen Verzeichnis zu arbeiten, z.B Dateien darin zu speichern oder es zu modifizieren.

## How to:
Mit Python ist es einfach, zu überprüfen, ob ein Verzeichnis existiert:

```Python
import os

# Verzeichnis, das überprüft wird
dir_path = '/pfad/zum/verzeichnis'

# Überprüfen, ob das Verzeichnis existiert
if os.path.isdir(dir_path):
    print("Das Verzeichnis existiert.")
else:
    print("Das Verzeichnis existiert nicht.")
```

Beispiel Ausgabe, falls das Verzeichnis existiert:
```
Das Verzeichnis existiert.
```

Beispiel Ausgabe, falls das Verzeichnis nicht existiert:
```
Das Verzeichnis existiert nicht.
```

## Deep Dive
Früher wurde oft `os.path.exists()` verwendet, um zu überprüfen, ob ein Pfad existiert, doch es kann irreführend sein, weil es `True` zurückgibt, egal ob der Pfad auf eine Datei oder ein Verzeichnis zeigt. `os.path.isdir()` ist spezifischer und wird bevorzugt, wenn man sicherstellen möchte, dass es sich um ein Verzeichnis handelt.

Alternativ kann `pathlib` aus der Python Standard Library genutzt werden, die einen objektorientierten Ansatz bietet:

```Python
from pathlib import Path

# Neues Path-Objekt
dir_path = Path('/pfad/zum/verzeichnis')

# Überprüfen, ob das Verzeichnis existiert
if dir_path.is_dir():
    print("Das Verzeichnis existiert.")
else:
    print("Das Verzeichnis existiert nicht.")
```

Diese Methode ist moderner und wird oft als klarer und pythonischer angesehen.

## See Also
Weitere Informationen und Beispiele finden Sie in der offiziellen Python-Dokumentation:
- os.path.isdir: https://docs.python.org/3/library/os.path.html#os.path.isdir
- pathlib.Path.is_dir: https://docs.python.org/3/library/pathlib.html#pathlib.Path.is_dir
