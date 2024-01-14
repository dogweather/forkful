---
title:    "Python: Erstellen einer temporären Datei"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit der Erstellung einer temporären Datei beschäftigen? Das Erstellen von temporären Dateien kann in verschiedenen Programmierszenarien nützlich sein, wie z.B. zum Speichern von Zwischenergebnissen, zum Debuggen von Code oder zum Erstellen von temporären Ordnerstrukturen.

## So geht's

Um eine temporäre Datei in Python zu erstellen, gibt es mehrere Möglichkeiten. Eine einfache Methode ist die Verwendung der `tempfile` Bibliothek. Hier ein Beispielcode, der eine temporäre Datei erstellt und einen Text in die Datei schreibt:

```Python
import tempfile

# Erstelle eine temporäre Datei
temp_file = tempfile.NamedTemporaryFile()

# Schreibe einen Text in die Datei
text = "Dies ist ein Beispieltext."
temp_file.write(text.encode())

# Drucke den Inhalt der Datei
print(temp_file.read())
```

Die Ausgabe des obigen Codes würde folgendermaßen aussehen:

```Python
b'Dies ist ein Beispieltext.'
```

Um die erstellte temporäre Datei zu verwenden, können Sie den Pfad zur Datei mit `temp_file.name` abrufen.

## Tiefere Einblicke

Wenn Sie sich tiefer mit dem Erstellen von temporären Dateien in Python beschäftigen möchten, gibt es einige wichtige Dinge zu beachten. Zum Beispiel können Sie beim Erstellen einer temporären Datei angeben, ob sie gleich gelöscht werden soll oder nicht. Dies kann mit dem Parameter `delete` in der `NamedTemporaryFile` Funktion angegeben werden. Außerdem können Sie festlegen, in welchem Modus die Datei geöffnet werden soll (z.B. zum Schreiben oder Lesen). Weitere Informationen dazu finden Sie in der offiziellen [Dokumentation](https://docs.python.org/3/library/tempfile.html).

## Siehe auch

- [Temporäre Dateien in Python erstellen](https://www.tutorialspoint.com/create-temporary-file-in-python)
- [Die `tempfile` Bibliothek in Python](https://www.geeksforgeeks.org/tempfile-in-python/)

Vielen Dank, dass Sie sich die Zeit genommen haben, diesen Blog-Beitrag zu lesen. Wir hoffen, dass er Ihnen dabei geholfen hat, ein besseres Verständnis für das Erstellen von temporären Dateien in Python zu erlangen. Happy coding!