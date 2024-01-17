---
title:                "Überprüfung ob ein Verzeichnis existiert"
html_title:           "Python: Überprüfung ob ein Verzeichnis existiert"
simple_title:         "Überprüfung ob ein Verzeichnis existiert"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Überprüfung, ob ein Verzeichnis existiert, ist ein wichtiger Aspekt der Programmierung, um sicherzustellen, dass das Programm reibungslos funktioniert. In der Regel wird dies verwendet, um zu vermeiden, dass das Programm abstürzt oder unerwartete Fehler auftreten.

## Wie geht's:

Ein Verzeichnis kann mithilfe der Funktion `os.path.exists()` in Python überprüft werden. Diese Funktion gibt `True` zurück, wenn das Verzeichnis existiert, andernfalls wird `False` zurückgegeben. Hier ist ein Beispiel für das Überprüfen des Verzeichnisses "Documents" auf einem Windows-System:

```python
import os
print(os.path.exists('C:/Users/User/Documents'))
```

Output: `True`

## Tiefer Einblick:

Die Überprüfung, ob ein Verzeichnis existiert, hat eine wichtige Rolle in der Evolution der Programmiersprachen gespielt. Frühere Sprachen wie C und C++ hatten keine integrierte Möglichkeit, um dies zu tun, daher mussten Entwickler kreative Workarounds finden. In Python ist es dank der `os`-Bibliothek viel einfacher geworden.

Es gibt auch alternative Methoden, um ein Verzeichnis zu überprüfen, wie zum Beispiel mit `os.path.isdir()`, das zusätzlich überprüft, ob es sich wirklich um ein Verzeichnis handelt, und nicht um eine Datei.

## Sieh auch:

Weitere Informationen und Beispiele finden Sie in der offiziellen Dokumentation von Python: https://docs.python.org/3/library/os.path.html

Weitere hilfreiche Ressourcen zur Programmierung mit Python finden Sie hier: https://realpython.com/