---
title:                "Python: Eine Textdatei schreiben"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Du magst vielleicht denken, dass das Schreiben einer Textdatei ein einfaches Konzept ist, aber es ist tatsächlich sehr nützlich in der Welt der Programmierung. Das Erstellen und Bearbeiten von Textdateien kann dir helfen, Daten zu speichern, zu organisieren oder zu teilen, was eine wichtige Fähigkeit für jeden Programmierer ist.

## Wie

Um eine Textdatei zu schreiben, musst du zunächst eine neue Datei erstellen. Mit Python kannst du dies mit dem Befehl ```open()``` tun. In diesem Beispiel nennen wir unsere Datei "meine_datei.txt":

```Python
f = open('meine_datei.txt', 'w')
```

Der zweite Parameter in der ```open()``` Funktion gibt an, dass wir die Textdatei zum Schreiben (`w` für "write") öffnen möchten. Nun kannst du mit dem Schreiben in die Datei beginnen. Hier ist ein Beispiel, wie man den Inhalt einer Datei definiert und in die erstellte Datei schreibt:

```Python
inhalt = "Hallo, dies ist ein Beispieltext."
f.write(inhalt)
```

Du kannst auch mehrere Zeilen in die Datei schreiben, indem du den Befehl ```write()``` mehrmals verwendest. Vergiss jedoch nicht, am Ende jeden Satzes ein Zeilenumbruchsymbol (`\n`) hinzuzufügen. Zum Beispiel:

```Python
inhalt1 = "Hallo, dies ist die erste Zeile."
inhalt2 = "Das ist die zweite Zeile."
f.write(inhalt1 + "\n" + inhalt2)
```

Nachdem du deinen Text geschrieben hast, solltest du die Datei wieder schließen, damit die Änderungen gespeichert werden:

```Python
f.close()
```

## Deep Dive

Es gibt viele zusätzliche Dinge, die du beim Schreiben einer Textdatei in Python beachten solltest. Zum Beispiel kannst du die Funktion ```writelines()``` verwenden, um eine Liste von Strings in die Datei zu schreiben, anstatt jeden einzelnen String mit ``write()`` zu schreiben.

Außerdem ist es wichtig, die Datei im richtigen Modus zu öffnen. Das `w` in unserem anfänglichen ```open()``` Befehl steht für "write" und überschreibt jeglichen existierenden Inhalt in der Datei. Wenn du die Datei öffnen und vorhandenen Inhalt behalten möchtest, solltest du stattdessen den Modus ```a``` für "append" verwenden.

Und wenn du nur den Inhalt einer Textdatei lesen möchtest, kannst du den Modus ```r``` für "read" verwenden.

## Siehe auch

- [Python-Dokumentation zu ```open()```](https://docs.python.org/de/3/library/functions.html#open)
- [Weitere Infos zur Textverarbeitung mit Python](https://www.python-kurs.eu/kurs.php?id=34)