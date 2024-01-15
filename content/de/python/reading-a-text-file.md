---
title:                "Eine Textdatei lesen"
html_title:           "Python: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung ist es unerlässlich, mit Textdateien umgehen zu können. Diese Art der Dateien enthält reine Textinformationen und ist daher universell einsetzbar. Ob Sie Daten speichern, analysieren oder bearbeiten möchten - das Lesen von Textdateien ist ein grundlegendes und wichtiger Bestandteil der Programmierung.

## Wie

Das Lesen von Textdateien in Python ist einfach und erfordert nur wenige Zeilen Code. Hier ist ein Beispiel:

```Python
# Öffnen und Lesen der Textdatei - in diesem Fall 'beispiel.txt'
f = open('beispiel.txt', 'r')
# Iterieren durch die Datei und Ausgabe jeder Zeile
for line in f:
    print(line)
# Schließen der Datei
f.close()
```

Die Output-Nachrichten werden in der Reihenfolge ausgegeben, in der sie in der Textdatei aufgeführt sind. Das Ausführen des obigen Codes auf einer Textdatei mit dem Inhalt "Dies ist ein Beispieltext." würde "Dies ist ein Beispieltext." ausgeben. Aufgrund der einfachen Syntax und der intuitiven Verwendung ist das Lesen von Textdateien in Python eine effiziente Möglichkeit, Informationen aus Dateien zu erhalten.

## Deep Dive

Beim Lesen von Textdateien in Python gibt es einige wichtige Dinge zu beachten. Standardmäßig wird die Datei im Nur-Lese-Modus ("r") geöffnet, was bedeutet, dass Sie keine Änderungen an der Datei vornehmen können. Wenn Sie jedoch Änderungen in der Datei vornehmen möchten, müssen Sie den Modus beim Öffnen der Datei angeben, z.B. "w" zum Schreiben oder "a" zum Anhängen an die Datei.

Eine weitere wichtige Überlegung ist die Behandlung von Zeilenumbrüchen. Unter Windows erfolgt dies durch "CRLF" (Carriage Return Line Feed), während es unter Unix-Systemen durch "LF" (Line Feed) dargestellt wird. Dies kann zu Problemen führen, wenn Sie die Datei in einem anderen Betriebssystem lesen. Aus diesem Grund ist es in Python immer empfehlenswert, die integrierte Funktion ".rstrip()" zu verwenden, um Leerzeichen und Zeilenumbrüche von den gelesenen Zeilen zu entfernen.

## Siehe auch

- Offizielle Python-Dokumentation zum Lesen und Schreiben von Dateien: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Nützliche Tipps zum Lesen und Schreiben von Dateien in Python: https://realpython.com/read-write-files-python/