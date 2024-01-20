---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Es geht um die Überprüfung, ob ein Verzeichnis in Ihrer Datei- oder Pfadstruktur existiert. Für Python-Entwickler ist dies entscheidend, um sicherzustellen, dass Dateioperationen wie Lesen und Schreiben reibungslos funktionieren, und um mögliche Pfadfehler zu vermeiden.

## So geht's:

Mit Python ist es ziemlich einfach zu überprüfen, ob ein Verzeichnis existiert. Dafür benutzen wir das os-Modul.

```Python
import os

# Angegebener Pfad
path = "/beispiel/pfad/"

# Überprüfung
if os.path.isdir(path):
    print("Das Verzeichnis existiert.")
else:
    print("Das Verzeichnis existiert nicht.")
```

Wenn der angegebene Pfad existiert, gibt das Programm "Das Verzeichnis existiert." aus. Andernfalls "Das Verzeichnis existiert nicht."

## Auf Tiefergehendes

Historisch gesehen gibt es die Funktion os.path.isdir() seit Python 1.5.2. Es ist eine direkte und einfache Methode zum Überprüfen des Vorhandenseins eines Verzeichnisses.

Es gibt Alternativen wie die Verwendung des pathlib-Moduls, das in Python 3.4 eingeführt wurde und objektorientiertere Methoden zur Pfadmanipulation bietet.

```Python
from pathlib import Path

path = Path("/beispiel/pfad/")
if path.is_dir():
    print("Das Verzeichnis existiert.")
else:
    print("Das Verzeichnis existiert nicht.")
```

Die Implementierung dieser Funktionen ist tief in den Betriebssystemaufrufen der jeweiligen Plattformen verankert, auf denen Python läuft. Daher ist ihre Verwendung konsistent und zuverlässig über alle Systeme hinweg.

## Siehe Auch:

1. Python os Modul: https://docs.python.org/3/library/os.path.html#os.path.isdir
2. Python pathlib Modul: https://docs.python.org/3/library/pathlib.html
3. Objektorientierte Pfadmanipulation: https://realpython.com/python-pathlib/