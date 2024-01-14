---
title:    "Python: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Warum

Beim Programmieren kommt es oft vor, dass man überprüfen muss, ob ein bestimmtes Verzeichnis auf dem Computer existiert. Dies kann aus verschiedenen Gründen notwendig sein, zum Beispiel um sicherzustellen, dass eine Datei erfolgreich gespeichert oder gelesen werden kann. Die Verwendung von Python ermöglicht es uns, diese Überprüfung einfach und effizient durchzuführen, weshalb es wichtig ist zu wissen, wie man dies umsetzt.

# Wie man überprüft, ob ein Verzeichnis existiert

Um zu überprüfen, ob ein Verzeichnis auf dem Computer vorhanden ist, können wir die `os` Library von Python verwenden. Diese enthält die Funktion `path.exists()`, welche uns die Möglichkeit gibt, den Pfad zu einem Verzeichnis anzugeben und zu überprüfen, ob dieses existiert oder nicht.

Schauen wir uns ein Beispiel an:

```Python
import os

dir_path = "C:/Users/Name/Desktop/Projekt/"

if os.path.exists(dir_path):
    print("Das Verzeichnis existiert!")
else:
    print("Das Verzeichnis existiert nicht!")
```

Hier haben wir zuerst die `os` Library importiert und dann einen Variablen `dir_path` zugewiesen, in der wir den Pfad zu unserem Verzeichnis angegeben haben. Anschließend überprüfen wir in einer `if`-Bedingung mit `os.path.exists()`, ob das Verzeichnis existiert und geben entsprechend eine Meldung aus.

# Tiefer Einblick

Ein wichtiger Aspekt beim Überprüfen von Verzeichnissen ist, dass es auch möglich ist, zu überprüfen, ob eine bestimmte Datei in diesem Verzeichnis vorhanden ist. Dafür können wir die Funktion `path.isfile()` verwenden, die ähnlich wie `path.exists()` funktioniert, aber zusätzlich noch prüft, ob es sich bei dem angegebenen Pfad um eine Datei handelt.

Zum Beispiel können wir folgenden Code nutzen, um zu überprüfen, ob eine `.txt` Datei in unserem Verzeichnis vorhanden ist:

```Python
import os

dir_path = "C:/Users/Name/Desktop/Projekt/"
file_name = "text.txt"

if os.path.isfile(dir_path + file_name):
    print("Die Datei existiert!")
else:
    print("Die Datei existiert nicht!")
```

# Siehe auch

- [Python `os` Library Dokumentation](https://docs.python.org/3/library/os.html)
- [Weitere Möglichkeiten zur Überprüfung von Dateien und Verzeichnissen in Python](https://www.geeksforgeeks.org/python-os-path-functions-yield/)
- [Tutorial zur Verwendung von Verzeichnissen in Python](https://www.datacamp.com/community/tutorials/python-system-path)