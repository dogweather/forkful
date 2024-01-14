---
title:                "Python: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals mit Datumsangaben in Ihrem Python-Projekt gearbeitet haben, sind Sie möglicherweise auf die Notwendigkeit gestoßen, ein Datum in eine Zeichenfolge (String) umzuwandeln. Dies ist besonders hilfreich, wenn Sie Daten in eine Textdatei schreiben oder sie auf eine bestimmte Weise formatieren möchten.

## Wie geht man vor

Um ein Datum in eine Zeichenfolge umzuwandeln, gibt es in Python die Funktion `strftime()`. Diese Funktion akzeptiert ein Datum- oder Zeitobjekt und ein Formatierungsargument und gibt eine Zeichenfolge entsprechend dem angegebenen Format zurück.

Um dies besser zu verstehen, werfen wir einen Blick auf einige Beispiele:

```Python
import datetime

# Aktuelles Datum und Uhrzeit abrufen
now = datetime.datetime.now()

# Datum in formatierte Zeichenfolge umwandeln
date_string = now.strftime('%d.%m.%Y')

print(type(date_string)) # Ausgabe: str
print(date_string) # Ausgabe: 28.05.2021
```

Wie im obigen Beispiel gezeigt, wird das aktuelle Datum in eine Zeichenfolge umgewandelt, die im Format von Tag.Monat.Jahr dargestellt wird. Sie können auch andere Formatierungen wie `%b` für die Kurzform des Monats oder `%A` für den vollen Wochentag verwenden. Eine vollständige Liste der verfügbaren Formate finden Sie in der [Python-Dokumentation](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior).

Wenn Sie ein Datum in einer anderen Sprache anzeigen möchten, können Sie dies auch mit der Funktion `setlocale()` tun, indem Sie das entsprechende Sprachkürzel angeben. Zum Beispiel, um das Datum auf Deutsch anzuzeigen:

```Python
import datetime
import locale

# Sprachkürzel für Deutsch festlegen
locale.setlocale(locale.LC_TIME, 'de_DE')

# Aktuelles Datum abrufen
now = datetime.datetime.now()

# Datum in formatierte Zeichenfolge umwandeln
date_string = now.strftime('%d %B %Y')

print(date_string) # Ausgabe: 28 Mai 2021
```

## Tiefer Einblick

Die Funktion `strftime()` ist Teil des [datetime](https://docs.python.org/3/library/datetime.html) Moduls in Python, das viele nützliche Methoden enthält, um mit Datum und Zeit umzugehen. Diese Funktion verwendet auch das [strftime()-Klassenmethode](https://docs.python.org/3/library/datetime.html#datetime.datetime.strftime), die den Zeitpunkt in ein lesbares Format umwandelt.

Umgekehrt gibt es auch die `strptime()` Funktion, die eine Zeichenfolge in ein Datum oder eine Uhrzeit umwandelt. Dies kann hilfreich sein, wenn Sie ein Datum aus einer Benutzereingabe in Ihr Programm einlesen möchten. Mehr Details dazu finden Sie auch in der [Python-Dokumentation](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior).

## Siehe auch

- [Python-Dokumentation zum datetime-Modul](https://docs.python.org/3/library/datetime.html)
- [Tutorials zur Verwendung von Datumsangaben in Python](https://realpython.com/python-dates/)
- [Video-Tutorial zu datetime in Python](https://www.youtube.com/watch?v=eirjjyP2qcQ)