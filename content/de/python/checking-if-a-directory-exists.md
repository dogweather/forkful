---
title:                "Python: Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist eine wichtige Funktion in der Python-Programmierung. Es ermöglicht uns, sicherzustellen, dass unser Code ordnungsgemäß auf die erforderlichen Dateien zugreifen kann, bevor wir versuchen, sie zu öffnen oder zu bearbeiten.

# Wie das funktioniert

Die Überprüfung eines Verzeichnisses ist relativ einfach und erfordert nur wenige Zeilen Code. Zunächst müssen wir das "os" Modul importieren, das uns Zugriff auf Betriebssystem-spezifische Funktionen bietet. Dann verwenden wir die Methode "path.exists()", um den Pfad zu unserem Verzeichnis anzugeben und zu überprüfen, ob es existiert.

```Python
import os

if os.path.exists("Verzeichnisname"):
  print("Das Verzeichnis existiert.")
else:
  print("Das Verzeichnis existiert nicht.")
```

Das obige Beispiel gibt je nach Ergebnis entweder "Das Verzeichnis existiert." oder "Das Verzeichnis existiert nicht." aus.

# Tiefere Einblicke

Wenn Sie tiefer in die Überprüfung von Verzeichnissen eintauchen möchten, gibt es einige zusätzliche Funktionen, die Ihnen helfen können, die Existenz eines Verzeichnisses zu überprüfen.

Zum Beispiel können Sie die Methode "path.isdir()" verwenden, um sicherzustellen, dass es sich bei dem angegebenen Pfad tatsächlich um ein Verzeichnis und nicht um eine Datei handelt. Sie können auch die Methode "path.islink()" verwenden, um zu überprüfen, ob es sich bei dem angegebenen Pfad um einen symbolischen Link handelt.

```Python
import os

pfad = "Verzeichnisname"

if os.path.exists(pfad):
  if os.path.isdir(pfad):
    print("Es handelt sich um ein Verzeichnis.")
  else:
    print("Es handelt sich um eine Datei.")
  if os.path.islink(pfad):
    print("Es ist ein symbolischer Link.")
else:
  print("Das Verzeichnis existiert nicht.")
```

Diese zusätzlichen Funktionen können nützlich sein, um die Art des Pfads zu bestimmen, den Sie überprüfen, und Ihre weiteren Schritte entsprechend anzupassen.

# Siehe auch

- [Python Documentation: os.path — Common pathname manipulations](https://docs.python.org/3/library/os.path.html)
- [Real Python: Working with Files in Python](https://realpython.com/working-with-files-in-python/)