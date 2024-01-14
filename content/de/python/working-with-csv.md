---
title:                "Python: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV-Dateien, auch bekannt als Comma-Separated Values, sind ein gebräuchliches Dateiformat für den Austausch von Daten zwischen verschiedenen Programmen. Sie können in Programmen wie Microsoft Excel oder Google Sheets geöffnet und bearbeitet werden, sind jedoch auch in der Programmierung sehr nützlich. In diesem Blogbeitrag werden wir uns ansehen, warum es sich lohnt, mit CSV-Dateien in Python zu arbeiten.

## Wie geht das?

Um mit CSV-Dateien in Python zu arbeiten, müssen wir zuerst das CSV-Modul importieren. Dann können wir die `reader()`-Funktion nutzen, um die Daten aus einer CSV-Datei zu lesen. Die Syntax ist dabei wie folgt:

```Python
import csv

# Öffnen der CSV-Datei
with open('beispiel.csv', 'r') as csv_datei:
    # Einlesen der Daten mit der reader() Funktion
    csv_reader = csv.reader(csv_datei)

    # Iteration über die Zeilen der CSV-Datei
    for zeile in csv_reader:
        # Ausgabe der Daten pro Zeile
        print(zeile)
```

Die Ausgabe sieht dann folgendermaßen aus:

```
['Name', 'Alter', 'Stadt']
['Anna', '25', 'Berlin']
['Max', '30', 'Hamburg']
['Lisa', '35', 'München']
```

Wir können auch die `writer()`-Funktion nutzen, um Daten in eine CSV-Datei zu schreiben. Hier ist ein Beispiel:

```Python
import csv

# Öffnen einer neuen CSV-Datei zum Schreiben
with open('neue_daten.csv', 'w') as csv_datei:
    # Erstellen eines csv_writer Objekts
    csv_writer = csv.writer(csv_datei)

    # Schreiben von Daten in die CSV-Datei
    csv_writer.writerow(['Name', 'Alter', 'Stadt'])
    csv_writer.writerow(['Peter', '40', 'Berlin'])
    csv_writer.writerow(['Sarah', '28', 'Hamburg'])
    csv_writer.writerow(['Tom', '33', 'München'])
```

Die neue Datei `neue_daten.csv` würde dann folgende Daten enthalten:

```
Name,Alter,Stadt
Peter,40,Berlin
Sarah,28,Hamburg
Tom,33,München
```

## Tiefentauchen

Das CSV-Modul bietet außerdem Funktionen zum Konvertieren von Daten in ein anderes Format, zum Beispiel mit der `DictReader()`-Funktion, die CSV-Daten in ein Dictionary umwandelt. Wir können auch spezifische Trennzeichen und Zeilenumbrüche angeben, die in der CSV-Datei verwendet werden.

Außerdem können wir mit der `delimiter()`-Funktion bestimmte Zellen aus der CSV-Datei auswählen, basierend auf der Position in der Datei oder auf bestimmten Kriterien wie einem bestimmten Wort oder Wert.

Es lohnt sich, sich mit den verschiedenen Funktionen des CSV-Moduls in Python auseinanderzusetzen, da sie viele nützliche Methoden bieten, um effektiv mit CSV-Dateien zu arbeiten.

## Siehe auch

- [Python CSV-Modul Dokumentation](https://docs.python.org/3/library/csv.html)
- [Tutorial: CSV-Dateien in Python lesen und schreiben](https://realpython.com/python-csv/)
- [Python: Wann sollte ich CSV verwenden?](https://realpython.com/python-csv/#when-should-you-use-csv)