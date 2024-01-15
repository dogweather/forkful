---
title:                "Zwei Daten vergleichen"
html_title:           "Python: Zwei Daten vergleichen"
simple_title:         "Zwei Daten vergleichen"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Bevor wir uns mit dem Vergleichen von zwei Daten in Python befassen, sollten wir uns die Frage stellen: Warum ist das überhaupt wichtig? Die Antwort darauf ist einfach: Als Programmierer*innen müssen wir oft mit Datumsangaben arbeiten und es ist wichtig zu wissen, wie man diese vergleicht, um komplexe Aufgaben zu lösen.

## Wie geht man vor

Für den Vergleich von zwei Daten in Python gibt es verschiedene Methoden. Im Folgenden werden wir zwei einfache und effektive Ansätze kennenlernen: mithilfe des `datetime`-Moduls und durch Umwandlung in ein Integer-Format. Schauen wir uns dazu ein Beispiel an:

```Python
# Beispiel 1: Vergleich mithilfe des datetime-Moduls
import datetime

date1 = datetime.date(2021, 5, 16)
date2 = datetime.date(2020, 8, 27)

if date1 > date2:
    print("Date 1 liegt später als Date 2")
elif date2 > date1:
    print("Date 2 liegt später als Date 1")
else:
    print("Beide Daten sind identisch")

# Beispiel 2: Vergleich durch Umwandlung in Integer-Format
date1 = 20210516
date2 = 20200827

if date1 > date2:
    print("Date 1 liegt später als Date 2")
elif date2 > date1:
    print("Date 2 liegt später als Date 1")
else:
    print("Beide Daten sind identisch")
```

**Output:**
```
Date 1 liegt später als Date 2
Date 1 liegt später als Date 2
```

Wie wir sehen, gibt es zwei verschiedene Wege, um zwei Daten miteinander zu vergleichen. In beiden Fällen wird die Vergleichsoperation (`>` oder `<`) verwendet, um festzustellen, welches Datum später liegt. Im ersten Beispiel wird dafür das `datetime`-Modul verwendet, während im zweiten Beispiel die Daten in das Integer-Format mit dem Jahr, dem Monat und dem Tag umgewandelt wurden.

## Grundlegender Vergleich vs. Tiefer Einblick

In dieser kurzen Einführung haben wir uns darauf konzentriert, wie man zwei Daten schnell und einfach miteinander vergleicht. Jedoch gibt es noch weitere Aspekte zu berücksichtigen, wie zum Beispiel das Vergleichen von Zeitangaben oder die Berücksichtigung von Zeitzonen. Für einen tieferen Einblick in das Vergleichen von Daten in Python empfehle ich deshalb folgende Ressourcen:

- [Python-Dokumentation zu Datums- und Zeitfunktionen](https://docs.python.org/de/3/library/datetime.html)
- [Tutorial zu Datums- und Zeitangaben in Python](https://www.programiz.com/python-programming/datetime)
- [Weitere Beispiele und Erklärungen für das Vergleichen von Daten in Python](https://www.geeksforgeeks.org/python-program-to-compare-two-datetimes/)

## Siehe auch

Weitere nützliche Links zum Thema Vergleichen von Daten in Python:

- [Offizielle Python-Dokumentation](https://www.python.org/)
- [Codecademy Python-Kurs](https://www.codecademy.com/learn/learn-python)
- [Stack Overflow - Fragen und Antworten zu Python](https://stackoverflow.com/questions/tagged/python)