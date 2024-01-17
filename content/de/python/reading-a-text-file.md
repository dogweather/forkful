---
title:                "Ein Textfile lesen"
html_title:           "Python: Ein Textfile lesen"
simple_title:         "Ein Textfile lesen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen einer Textdatei ist ein häufiges und grundlegendes Programmierkonzept, bei dem der Inhalt einer Datei in ein Computerprogramm geladen wird. Programmierer tun dies, um Daten zu verarbeiten, die in Textform gespeichert sind, beispielsweise in einer CSV-Datei oder einem einfachen Textdokument.

## Wie geht das?

```Python
with open('beispiel.txt') as f:
  for line in f:
    print(line)
```

In diesem Beispiel öffnen wir die Datei "beispiel.txt" und speichern sie in der Variablen "f". Mit der ```for```-Schleife können wir dann jede Zeile aus der Datei nacheinander ausgeben. Dieser Code funktioniert sowohl für einfache Textdateien als auch für CSV-Dateien.

## Tiefere Einblicke

Das Lesen von Textdateien hat eine lange Geschichte, die bis in die Anfänge des Computerzeitalters zurückreicht. Textdateien waren damals das Hauptmedium für die Speicherung von Daten, da sie einfach zu erstellen und zu bearbeiten waren. Heutzutage gibt es viele Alternativen zum Lesen von Textdateien, wie zum Beispiel Datenbanken oder spezialisierte Dateiformate. 

In Python gibt es mehrere Möglichkeiten, Textdateien zu lesen. Wie im obigen Beispiel können Sie die eingebaute Funktion ```open``` verwenden oder die Module ```csv``` oder ```pandas``` importieren.

## Siehe auch

- [Python Dokumentation zu Dateien lesen](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Real Python: Reading and Writing Files in Python](https://realpython.com/read-write-files-python/)
- [Wikipedia: Text file](https://en.wikipedia.org/wiki/Text_file)