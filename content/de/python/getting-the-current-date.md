---
title:                "Python: Aktuelles Datum erhalten"
simple_title:         "Aktuelles Datum erhalten"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Ermitteln des aktuellen Datums kann in der Programmierung nützlich sein, um bestimmte Funktionen oder Anweisungen im Code basierend auf dem Datum auszuführen. Zum Beispiel könnte ein Programmierer ein Skript schreiben, das eine Erinnerungsnachricht anzeigt, wenn das heutige Datum ein bestimmtes Datum erreicht.

## Wie man das aktuelle Datum in Python erhält

Um das aktuelle Datum in Python zu erhalten, können wir das Modul "datetime" verwenden. Zunächst müssen wir das Modul importieren, indem wir den folgenden Code am Anfang unseres Skripts hinzufügen:

```Python
import datetime
```

Dann können wir die Funktion "datetime.now()" verwenden, um das aktuelle Datum und die Uhrzeit zurückzugeben. Zum Beispiel:

```Python
heutiges_datum = datetime.now()
print(heutiges_datum)
```

Die Ausgabe wird in folgendem Format angezeigt: "YYYY-MM-DD HH:MM:SS.MS". Wenn wir nur das Datum ohne die Uhrzeit benötigen, können wir die Funktion "date()" verwenden:

```Python
heutiges_datum = datetime.now().date()
print(heutiges_datum)
```

Die Ausgabe in diesem Fall lautet "YYYY-MM-DD". Mit diesen Funktionen können wir das aktuelle Datum und die Uhrzeit in verschiedensten Formaten erfassen.

## Tiefgehender Einblick

Das Modul "datetime" bietet noch viele weitere Funktionen für die Arbeit mit Datums- und Zeitangaben. Beispielsweise können wir mit der Funktion "strftime()" das Datum in ein vom Benutzer angegebenes Format umwandeln. In der offiziellen Dokumentation von Python finden Sie alle verfügbaren Funktionen mit ausführlichen Erklärungen und Beispielen.

## Siehe auch

- Offizielle Python-Dokumentation zum Modul "datetime": https://docs.python.org/3/library/datetime.html
- Ein Tutorial zum Arbeiten mit Datums- und Zeitangaben in Python: https://realpython.com/python-datetime/
- Einführung in die Verwendung von Datums- und Zeitangaben in Python: https://www.programiz.com/python-programming/datetime