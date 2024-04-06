---
date: 2024-01-20 17:54:56.193566-07:00
description: "Wie geht das? Das Lesen von Textdateien ist Grundlage der Programmierung.\
  \ Es wurde schon in fr\xFChesten Programmiersprachen unterst\xFCtzt. Heutzutage\
  \ nutzen\u2026"
lastmod: '2024-04-05T21:53:55.351503-06:00'
model: gpt-4-1106-preview
summary: Das Lesen von Textdateien ist Grundlage der Programmierung.
title: Textdatei einlesen
weight: 22
---

## Wie geht das?
```Python
# Textdatei öffnen und lesen
with open('beispiel.txt', 'r') as datei:
    inhalt = datei.read()
    print(inhalt)

# Ausgabe
"""
Das ist der Inhalt der Textdatei.
Mehrere Zeilen können hier stehen.
"""
```

## Deep Dive
Das Lesen von Textdateien ist Grundlage der Programmierung. Es wurde schon in frühesten Programmiersprachen unterstützt. Heutzutage nutzen wir oft eingebaute Funktionen wie `open()` und Kontextmanager (`with`), um Ressourcenmanagement zu vereinfachen und Dateien automatisch zu schließen.

Die Alternativen zum Lesen von Dateiinhalten hängen von der Aufgabe ab. Zum Beispiel, um große Dateien zu verarbeiten, kann man `readline()` oder `readlines()` verwenden, um sie zeilenweise zu lesen. Für Binärdateien würde man den Modus auf 'rb' setzen.

Beim Lesen von Textdateien sollten wir auf die Zeichenkodierung achten. Standard ist UTF-8, aber manchmal sind Dateien anders kodiert.

## Siehe auch
- Python Dokumentation für `open()`: https://docs.python.org/3/library/functions.html#open
- W3Schools Tutorial zum Lesen von Dateien: https://www.w3schools.com/python/python_file_open.asp
- Real Python Tutorial über Datei I/O: https://realpython.com/read-write-files-python/
