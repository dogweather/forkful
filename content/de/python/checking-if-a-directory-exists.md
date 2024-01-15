---
title:                "Prüfen, ob ein Verzeichnis existiert."
html_title:           "Python: Prüfen, ob ein Verzeichnis existiert."
simple_title:         "Prüfen, ob ein Verzeichnis existiert."
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist eine wichtige Aufgabe in der Python-Programmierung. Sie ermöglicht es uns, unser Programm zu optimieren und Fehler zu vermeiden. Zum Beispiel können wir vermeiden, dass unser Code versucht, auf ein nicht vorhandenes Verzeichnis zuzugreifen, was zu Abstürzen oder Fehlern führen kann.

## Wie das geht

Der einfachste Weg, um zu überprüfen, ob ein Verzeichnis existiert, ist die Verwendung der Funktion `os.path.exists()`. Dies erfordert jedoch, dass wir das Modul `os` zu unserem Code hinzufügen.

``` python
import os

if os.path.exists("Verzeichnis"):
    print("Das Verzeichnis existiert.")
else:
    print("Das Verzeichnis existiert nicht.")
```

Dieses Code-Beispiel zeigt, wie wir die `os.path.exists()` Funktion verwenden, um zu überprüfen, ob das Verzeichnis mit dem Namen "Verzeichnis" existiert. Wenn das Verzeichnis existiert, wird die Meldung "Das Verzeichnis existiert." ausgegeben. Andernfalls wird die Meldung "Das Verzeichnis existiert nicht." ausgegeben.

## Tiefergehende Information

Das Überprüfen, ob ein Verzeichnis existiert, kann auch nützlich sein, um zu überprüfen, ob ein bestimmtes Verzeichnis bereits erstellt wurde, bevor wir versuchen, es zu erstellen. Wir können auch weitere Informationen über das Verzeichnis abrufen, wie zum Beispiel den Pfad, die Größe oder das Erstellungsdatum, indem wir verschiedene Funktionen aus dem `os` Modul verwenden. Weitere Informationen dazu finden Sie in der offiziellen [Python-Dokumentation](https://docs.python.org/3/library/os.path.html).

## Siehe auch

- [Überblick über das os.path Modul](https://realpython.com/python-pathlib/)
- [Tutorial über das os Modul](https://www.python-kurs.eu/python3_dateien_verzeichnisse.php)
- [Interaktives Einführungstutorial von Real Python](https://realpython.com/absolute-vs-relative-python-imports/)