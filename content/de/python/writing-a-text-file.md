---
title:                "Eine Textdatei schreiben"
date:                  2024-01-19
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"

category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Textdateien schreiben bedeutet, Informationen in einer Datei auf dem Datenträger zu speichern. Programmierer tun dies, um Daten zwischenzuspeichern, Konfigurationen zu notieren oder Informationen für andere Programme und Benutzer bereitzustellen.

## Wie geht das:
```Python
# Textdatei erstellen und schreiben
with open('beispiel.txt', 'w', encoding='utf-8') as datei:
    datei.write('Hallo, Welt!')
    
# Inhalt der Datei ausgeben
with open('beispiel.txt', 'r', encoding='utf-8') as datei:
    inhalt = datei.read()
    print(inhalt)
```
Ausgabe:
```
Hallo, Welt!
```

## Deep Dive
Das Schreiben von Textdateien ist so alt wie die Informatik selbst. Historisch gesehen waren Textdateien das primäre Medium für dauerhafte Datenspeicherung. Alternativen wie Datenbanken bieten strukturierte Abfrage- und Speichermöglichkeiten, sind aber komplexer. In Python nutzt der `open`-Befehl die Unterstützung des Betriebssystems, um Dateien zu lesen und zu schreiben. Dabei können verschiedene Modi (Lese-, Schreibzugriff, etc.) und Kodierungen (z.B. UTF-8) angegeben werden.

## Siehe auch
- Python Dokumentation zu Datei-Operationen: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Tutorial zum Umgang mit Dateien in Python: https://realpython.com/read-write-files-python/
- Offizielle Python Einführung in das Arbeiten mit Dateien: https://docs.python.org/3/library/io.html
