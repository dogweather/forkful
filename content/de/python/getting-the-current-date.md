---
title:    "Python: Das aktuelle Datum erhalten"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Warum

Das Abrufen des aktuellen Datums ist eine häufige Aufgabe in der Programmierung. Es kann dazu verwendet werden, die aktuelle Zeit für die Anzeige in einem Programm oder zur Überprüfung von Verfallsdaten zu erhalten.

# Wie man das aktuelle Datum in Python erhält

Um das aktuelle Datum in Python zu erhalten, können wir das `datetime`-Modul verwenden. Zuerst müssen wir das Modul importieren:

```Python
import datetime
```

Dann können wir die `today()`-Methode aufrufen, um das aktuelle Datum zu erhalten:

```Python
today = datetime.date.today()
```

Wir können auch das aktuelle Datum mit der `now()`-Methode erhalten, die auch die aktuelle Uhrzeit enthält:

```Python
now = datetime.datetime.now()
```

Um das Datum in einem bestimmten Format anzuzeigen, können wir die `strftime`-Methode verwenden:

```Python
formatted_date = today.strftime("%d-%m-%Y")
```

Das obige Beispiel würde das Datum im Format "Tag-Monat-Jahr" anzeigen. Eine vollständige Liste der Formatierungsoptionen für die `strftime`-Methode finden Sie in der offiziellen [Python-Dokumentation](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior).

# Tiefere Einblicke

Python verfügt auch über das `calendar`-Modul, mit dem wir einen Kalender für ein bestimmtes Jahr und einen bestimmten Monat erstellen können. Um zu überprüfen, welcher Tag in der Woche das aktuelle Datum ist, können wir die `weekday()`-Methode verwenden:

```Python
import calendar

weekday = calendar.day_name[today.weekday()]
```

Die obige Methode gibt eine ganze Zahl zurück, wobei 0 für Montag, 1 für Dienstag usw. steht. Die `day_name`-Liste enthält die entsprechenden Wochentagsnamen, aus denen wir den richtigen Namen für das aktuelle Datum abrufen können.

# Siehe auch

- Offizielle Python-Dokumentation zur [datetime](https://docs.python.org/3/library/datetime.html) und [calendar](https://docs.python.org/3/library/calendar.html) Module.
- Ein [Tutorial zur Verwendung von Datums- und Uhrzeitfunktionen in Python](https://realpython.com/python-datetime/).
- [Stack Overflow](https://stackoverflow.com/questions/tagged/datetime) Fragen und Antworten zu Python-Datum und Uhrzeit.