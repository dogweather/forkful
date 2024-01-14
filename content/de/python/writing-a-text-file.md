---
title:    "Python: Eine Textdatei schreiben"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Textdateien ist eine grundlegende Fähigkeit für jeden Programmierer. Textdateien dienen als einfache und effektive Möglichkeit, Daten zu speichern und zu teilen. Sie ermöglichen es uns, strukturierte Informationen in einem bestimmten Format zu speichern, das von anderen Programmen gelesen und verarbeitet werden kann. In diesem Blog-Beitrag werden wir uns einige der Gründe ansehen, warum das Schreiben von Textdateien für jeden Python-Programmierer wichtig ist.

## Wie man Textdateien schreibt

Das Schreiben von Textdateien in Python ist sehr einfach. Wir können dies mit der `open()` Funktion und dem Modus 'w' (für Schreiben) tun. Hier ist ein Beispiel:

```Python
# Öffnen der Datei im Schreibmodus
datei = open("neue_datei.txt", "w")

# Schreiben in die Datei
datei.write("Dies ist meine erste Textdatei, die ich mit Python geschrieben habe!")

# Schließen der Datei
datei.close()
```

Dieses Beispiel erstellt eine neue Textdatei namens "neue_datei.txt" und schreibt den angegebenen Text in die Datei. Beachten Sie, dass der Modus "w" jede vorhandene Datei mit demselben Namen überschreiben wird. Wir können auch den Modus "a" (für Anhängen) verwenden, um den Text am Ende der Datei hinzuzufügen.

Um Zeilenumbrüche in unsere Textdatei einzufügen, können wir die `write()` Funktion mit der escape-Sequenz `\n` verwenden. Beispiel:

```Python
# Öffnen der Datei im Anhänge-Modus
datei = open("neue_datei.txt", "a")

# Schreiben in die Datei
datei.write("\nDies ist die nächste Zeile in meiner Textdatei.")

# Schließen der Datei
datei.close()
```

Das Ergebnis in unserer Textdatei würde so aussehen:

```
Dies ist meine erste Textdatei, die ich mit Python geschrieben habe!
Dies ist die nächste Zeile in meiner Textdatei.
```

## Tief in die Materie eintauchen

Die `open()` Funktion akzeptiert auch einen optionalen Parameter "encoding", der angibt, welche Zeichencodierung verwendet werden soll. Wenn wir keine Zeichencodierung angeben, wird standardmäßig die Codierung des Betriebssystems verwendet. Wir können auch den Modus 'r' (lesen) verwenden, um eine Textdatei zu lesen, und die `read()` Funktion verwenden, um den Inhalt der Datei in eine Variable zu speichern. Beispiel:

```Python
# Öffnen der Datei im Lese-Modus
datei = open("textdatei.txt", "r")

# Speichern des Inhalts in einer Variablen
inhalt = datei.read()

# Schließen der Datei
datei.close()
```

Wir können auch ein for-Schleifenkonstrukt verwenden, um jede Zeile in der Datei einzeln zu lesen. Beispiel:

```Python
# Öffnen der Datei im Lese-Modus
datei = open("textdatei.txt", "r")

# Iterieren durch jede Zeile in der Datei
for zeile in datei:
  print(zeile)

# Schließen der Datei
datei.close()
```

## Siehe auch

- [Python-Dokumentation zu Textdateien](https://docs.python.org/de/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Real Python-Artikel zum Lesen und Schreiben von Textdateien in Python](https://realpython.com/read-write-files-python/)
- [Einführung in die Zeichenkodierung von Wikipedia](https://de.wikipedia.org/wiki/Zeichenkodierung)