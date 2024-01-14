---
title:                "Python: Eine Textdatei lesen"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Das Lesen von Textdateien ist eine grundlegende Fähigkeit, die jeder, der Python programmiert, beherrschen sollte. Textdateien sind eine häufige Form von Datenspeicherung und können in vielen verschiedenen Szenarien verwendet werden. In diesem Blog-Beitrag werden wir uns damit beschäftigen, wie man Textdateien in Python lesen kann.

## Wie
Das Lesen von Textdateien in Python ist relativ einfach und erfordert nur wenige Zeilen Code. Zunächst müssen wir jedoch die Datei öffnen, die wir lesen möchten, indem wir die `open()` Funktion verwenden. Wir müssen den Dateipfad und den Modus angeben, in dem wir die Datei öffnen möchten, zum Beispiel "r" für Lesen.

```
file = open("beispiel.txt", "r")
```

Als nächstes können wir die `read()` Funktion verwenden, um den Inhalt der Datei zu lesen und in eine Variable zu speichern. Wir können dann die Datei schließen, indem wir die `close()` Funktion verwenden.

```
inhalt = file.read()
file.close()
```

Die Variable `inhalt` enthält nun den gesamten Inhalt der gelesenen Datei. Wir können auch spezifische Zeilen lesen, indem wir die `readlines()` Funktion verwenden, die eine Liste mit allen Zeilen der Datei zurückgibt.

```
datei = open("beispiel.txt", "r")
zeilen = datei.readlines()
datei.close()
```

Wir können auch die `with` Anweisung verwenden, um den Code zu vereinfachen und sicherzustellen, dass die Datei automatisch geschlossen wird.

```
with open("beispiel.txt", "r") as datei:
    inhalt = datei.read()
```

## Deep Dive
Es gibt verschiedene Methoden zum Lesen von Textdateien in Python, wie z.B. die `readline()` Funktion, mit der wir Zeile für Zeile lesen können, oder die `seek()` Funktion, mit der wir an bestimmte Positionen in der Datei springen können. Wir können auch die `for` Schleife verwenden, um durch die Datei zu iterieren und jede Zeile einzeln zu verarbeiten.

Beachten Sie auch, dass wir den Modus der Datei angeben können, in dem wir sie öffnen möchten, z.B. "w" für Schreiben oder "a" für Anfügen.

## Siehe auch
- [Python Dokumentation zum Lesen von Dateien](https://docs.python.org/de/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Weitere Beispiele und Erklärungen zu Lesen von Textdateien in Python](https://www.w3schools.com/python/python_file_read.asp)
- [Tutorial zum Umgang mit Textdateien in Python](https://realpython.com/read-write-files-python/)