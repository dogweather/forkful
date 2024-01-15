---
title:                "Das heutige Datum erhalten"
html_title:           "Python: Das heutige Datum erhalten"
simple_title:         "Das heutige Datum erhalten"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum

Das Abrufen des aktuellen Datums ist eine grundlegende Aufgabe in der Python-Programmierung. Es hilft dabei, aktuelle Daten in Protokollen, Dateinamen oder anderen Anwendungen zur Verwaltung von Daten zu verwenden.

# Wie man das aktuelle Datum erhält

## Auf einfache Weise das aktuelle Datum erhalten

Um das aktuelle Datum in Python zu erhalten, verwenden Sie das Modul "datetime" und die Funktion "datetime.now()". Dies gibt Ihnen ein Objekt zurück, das das aktuelle Datum und die Uhrzeit enthält. Probieren Sie es selbst aus:

```python
import datetime

current_date = datetime.datetime.now()
print(current_date)
```

### Ausgabe

```python
2021-05-28 14:30:00.461602
```

## Formatieren des aktuellen Datums

Sie können das zurückgegebene Objekt auch formatieren, um das Datum in verschiedenen Formaten anzuzeigen. Hier ist ein Beispiel, das das Datum im Format "Tag-Monat-Jahr" anzeigt:

```python
current_date = datetime.datetime.now()
formatted_date = current_date.strftime("%d-%b-%Y")
print(formatted_date)
```

### Ausgabe

```python
28-May-2021
```

## Erhalten des aktuellen Datums mit der Zeitzone

Wenn Sie das aktuelle Datum und die Uhrzeit in einer bestimmten Zeitzone benötigen, können Sie das Modul "pytz" verwenden. Hier ist ein Beispiel, das das aktuelle Datum und die Uhrzeit in der Pacific-Standard-Zeit (PST) zurückgibt:

```python
import datetime
import pytz

pst_timezone = pytz.timezone('US/Pacific')
current_date = datetime.datetime.now(pst_timezone)
print(current_date)
```

### Ausgabe

```python
2021-05-28 05:30:00.461602-07:00
```

# Tiefere Einblicke

Das zurückgegebene Objekt durch die Funktion "datetime.now()" ist vom Typ "datetime.datetime", was bedeutet, dass es sowohl das Datum als auch die Uhrzeit enthält. Sie können auf verschiedene Attribute dieses Objekts zugreifen, z.B. das Jahr, den Monat, den Tag, die Stunde, die Minute oder die Sekunde.

Hier sind einige Beispiele:

```python
current_date = datetime.datetime.now()
print(f"Jahr: {current_date.year}")
print(f"Monat: {current_date.month}")
print(f"Tag: {current_date.day}")
print(f"Stunde: {current_date.hour}")
print(f"Minute: {current_date.minute}")
print(f"Sekunde: {current_date.second}")
```

### Ausgabe

```python
Jahr: 2021
Monat: 5
Tag: 28
Stunde: 14
Minute: 30
Sekunde: 0
```

Sie können auch mathematische Operationen auf dem Objekt "datetime.datetime" ausführen, z.B. das Hinzufügen oder Subtrahieren von Tagen. Hier ist ein Beispiel, das das Datum von morgen berechnet:

```python
current_date = datetime.datetime.now()
one_day = datetime.timedelta(days=1)
tomorrow = current_date + one_day
print(tomorrow)
```

### Ausgabe

```python
2021-05-29 14:30:00.461602
```

# Siehe auch

- [Offizielle Dokumentation zu "datetime" in Python](https://docs.python.org/3/library/datetime.html)
- [Offizielle Dokumentation zu "pytz" in Python](https://pypi.org/project/pytz/)
- [Weitere Informationen zu Datum und Zeit in Python](https://realpython.com/python-datetime/)