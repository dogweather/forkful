---
title:                "Python: Überprüfung, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfung, ob ein Verzeichnis vorhanden ist"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Manchmal kann es vorkommen, dass man in der Programmierung überprüfen muss, ob ein bestimmtes Verzeichnis existiert. Dies kann hilfreich sein, um sicherzustellen, dass der Code reibungslos funktioniert und um Fehler zu vermeiden.

## Wie geht's

Um zu überprüfen, ob ein Verzeichnis existiert, können wir die `os` Bibliothek in Python nutzen. Zuerst müssen wir diese importieren:

```Python
import os
```

Als nächstes können wir die Methode `path.isdir()` verwenden, um zu überprüfen, ob ein Verzeichnis existiert. Wir geben den Pfad des Verzeichnisses als Parameter ein und die Methode gibt `True` oder `False` zurück, je nachdem ob das Verzeichnis existiert oder nicht.

```Python
if os.path.isdir("mein_verzeichnis"):
    print("Das Verzeichnis existiert!")
else:
    print("Das Verzeichnis existiert nicht.")
```

Dieser Codeblock überprüft, ob ein Verzeichnis namens `mein_verzeichnis` im aktuellen Ordner existiert und gibt entsprechend eine Nachricht aus.

## Tiefer eingetaucht

Manchmal möchten wir nicht nur herausfinden, ob ein Verzeichnis existiert, sondern auch weitere Informationen darüber sammeln. Dafür können wir die Methode `path.join()` verwenden, um den Pfad des Verzeichnisses zu erstellen, `path.exists()` um zu überprüfen, ob der Pfad existiert, und `path.isabs()` um zu überprüfen, ob es sich um einen absoluten Pfad handelt.

```Python
# Verzeichnis-Pfad erstellen
verzeichnis_pfad = os.path.join("mein_verzeichnis", "unterverzeichnis")

# Überprüfen, ob der Pfad existiert
if os.path.exists(verzeichnis_pfad):
    print("Der Pfad existiert.")

# Überprüfen, ob es sich um einen absoluten Pfad handelt
if os.path.isabs(verzeichnis_pfad):
    print("Es handelt sich um einen absoluten Pfad.")
```

Dies sind nur einige Beispiele, wie man tiefer in die Überprüfung von Verzeichnissen eintauchen kann. Durch die Verwendung von Methoden aus der `os` Bibliothek gibt es noch viele weitere Möglichkeiten und Funktionen, um Verzeichnisse zu überprüfen und zu verarbeiten.

## Siehe auch

- [Dokumentation der os Bibliothek in Python](https://docs.python.org/3/library/os.html)
- [Weitere Beispiele zur Überprüfung von Verzeichnissen in Python](https://stackoverflow.com/questions/89228/calling-an-external-command-in-python)