---
title:    "Python: Einen Textdatei lesen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

Warum: Lesen von Textdateien kann nützlich sein, um Daten oder Informationen aus externen Quellen in einem Python-Programm zu nutzen oder zu verarbeiten.

Wie geht es: 
Das Lesen von Textdateien in Python ist relativ einfach und erfordert nur wenige Schritte. Zuerst müssen wir eine Verbindung zur Textdatei herstellen, dann die Daten auslesen und schließlich die Verbindung schließen.

```Python
datei = open('daten.txt','r') # Öffnet die Textdatei im Lesemodus und speichert sie in der Variable 'datei'
daten = datei.read() # Liest die Daten aus der Datei und speichert sie in der Variable 'daten'
print(daten) # Gibt die Daten aus
datei.close() # Schließt die Verbindung zur Datei
```

Das obige Beispiel zeigt, wie wir eine Textdatei mit dem Namen "daten.txt" öffnen, die Daten auslesen und schließlich die Verbindung schließen. Wir können auch die "with" Anweisung verwenden, um die Verbindung zur Datei automatisch zu schließen, sobald der Codeblock beendet ist.

```Python
with open('daten.txt', 'r') as datei: # Öffnet die Textdatei und speichert sie in der Variable 'datei'
    daten = datei.read() # Liest die Daten und speichert sie in der Variable 'daten'
    print(daten) # Gibt die Daten aus
```

Tief tauchen: 
Beim Lesen von Textdateien in Python gibt es einige wichtige Dinge zu beachten. Zum Beispiel müssen wir angeben, in welchem Modus wir die Datei öffnen möchten (Lesen, Schreiben, Anhängen usw.), da dies Auswirkungen auf die Art und Weise hat, wie wir mit der Datei interagieren können.

Außerdem müssen wir bei der Verwendung der "read()" Methode berücksichtigen, dass sie die gesamte Datei auf einmal einliest und in einer einzelnen Zeichenfolge speichert. Wenn die Datei sehr groß ist, kann dies zu Leistungsproblemen führen. In diesem Fall können wir die "readline()" Methode verwenden, um die Datei zeilenweise zu lesen, oder die "readlines()" Methode, um eine Liste mit allen Zeilen in der Datei zu erhalten.

Siehe auch: 
- [Python Dokumentation zum Öffnen von Dateien](https://docs.python.org/3/library/functions.html#open)
- [Python Dokumentation zu Dateiobjekten](https://docs.python.org/3/library/io.html#io.RawIOBase)
- [Python Tutorial zur Arbeit mit Dateien](https://realpython.com/read-write-files-python/)