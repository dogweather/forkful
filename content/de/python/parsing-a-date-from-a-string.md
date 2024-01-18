---
title:                "Ein Datum aus einem Text extrahieren"
html_title:           "Python: Ein Datum aus einem Text extrahieren"
simple_title:         "Ein Datum aus einem Text extrahieren"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String ist ein häufiges Problem, das Programmierer lösen müssen. Es bezieht sich auf die Umwandlung eines Datums in einem bestimmten Format, das als String vorliegt, in eine für den Computer verständliche Datumsdarstellung. Programmierer tun dies, um mit Datumsangaben in ihren Programmen arbeiten zu können.

## So geht's:
```Python
from datetime import datetime

date_string = "12.05.2020" # Beispielformat DD.MM.YYYY
parsed_date = datetime.strptime(date_string, '%d.%m.%Y')
print(parsed_date)
```

Die Ausgabe wäre: 2020-05-12 00:00:00

## Tiefes Eintauchen:
Das Parsen von Datumsangaben aus Strings ist ein Problem, das seit langem von Programmierern gelöst werden musste. Früher war dies eine zeitaufwändige Aufgabe, da jedes Programm eine eigene Lösung hatte. Heutzutage gibt es jedoch viele Programmierbibliotheken, die das Parsen von Datumsangaben vereinfachen. Eine alternative Methode ist die Verwendung von regulären Ausdrücken, um das Datum aus dem String zu extrahieren. Die Implementierung des Dateiparsens ist in Python durch die Bibliothek ```datetime``` möglich.

## Siehe auch:
Weitere Informationen zum Parsen von Datumsangaben in Python finden Sie in der offiziellen Dokumentation unter: https://docs.python.org/3/library/datetime.html