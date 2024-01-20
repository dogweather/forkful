---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen einer Textdatei ist die Fähigkeit eines Programms, Zeichenketten aus einer externen Datei zu erfassen und zu interpretieren. Programmierer machen dies, um Informationen zu bekommen oder zu speichern, ohne eine Datenbank zu benötigen. 

## So geht's:

```Python
# Datei Öffnen
file = open('beispiel.txt', 'r')

# Inhalt Lesen
inhalt = file.read()

# Ausgeben des Inhalts
print(inhalt)

# Datei Schließen
file.close()
```

Wenn `beispiel.txt` den Text `"Hallo, Welt!"` enthält, wäre die Ausgabe:

```Python
Hallo, Welt!
```

## Vertiefung

Das Lesen von Textdateien ist eine lange etablierte Praxis. Es datiert zurück bis zu den Anfängen der Informatik, als Speicher sehr begrenzt war. Es gibt Alternativen, einschließlich binärer Dateien und Datenbanken, aber Textdateien bleiben aufgrund ihrer Einfachheit und Portabilität populär. 

Die Standardmethode read() öffnet, liest und schließt die Datei. Es gibt jedoch auch andere Methoden wie readline(), die nur eine Zeile auf einmal liest, was nützlich ist, wenn man große Dateien bearbeitet, die den RAM belasten könnten. 

Die open()-Methode hat verschiedene Modi, wobei 'r' für Lesen steht. Andere Modi sind 'w' für Schreiben (löscht vorhandene Dateien), 'a' für Anhängen, 'r+' für Lesen und Schreiben und 'x' zum Erzeugen einer neuen Datei.

## Siehe auch

- [Offizielle Python-Dokumentation zum Umgang mit Dateien](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Blog-Artikel: Praxistipps für den Umgang mit Dateien in Python](https://realpython.com/read-write-files-python/)
- [Tutorial: Lesen von CSV- und Excel-Dateien in Python](https://www.datacamp.com/community/tutorials/pandas-read-csv)