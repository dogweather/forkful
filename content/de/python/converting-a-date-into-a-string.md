---
title:    "Python: Umwandlung eines Datums in einen String"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum
Das Konvertieren von Dateien in Strings kann sehr hilfreich sein, wenn Sie Daten in einem bestimmten Format speichern oder anzeigen möchten. Zum Beispiel könnten Sie die aktuelle Datum in einem Textdokument oder in einer Benutzeroberfläche anzeigen lassen.

## Wie man es macht
Das Konvertieren eines Datums in einen String kann in Python leicht mit der `strftime()` Funktion erreicht werden. Diese Funktion nimmt zwei Argumente an: ein Format, in dem das Datum angezeigt werden soll, und das Datum selbst. Zum Beispiel:

```Python
import datetime

# aktuelles Datum abrufen
heute = datetime.datetime.now()

# Datum in gewünschtes Format konvertieren
datum_str = heute.strftime("%d.%m.%Y")

# Ausgabe des Datums als String
print(datum_str) # 10.05.2021
```

Hier haben wir die aktuelle Zeit in das Format "Tag.Monat.Jahr" konvertiert. Es gibt viele verschiedene Optionen für das Formatieren eines Datums, je nachdem, welche Informationen Sie angezeigt haben möchten. Eine vollständige Liste der Formatierungsoptionen finden Sie in der [offiziellen Python-Dokumentation](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes).

## Tieferer Einblick
Die `strftime()` Funktion basiert auf der `datetime` Klasse in Python. Diese Klasse bietet viele hilfreiche Methoden zum Arbeiten mit Datums- und Zeitinformationen. Sie können zum Beispiel auch die `strptime()` Funktion verwenden, um einen String in ein Datum umzuwandeln, oder die `timedelta` Klasse, um verschiedene Zeitintervalle zu berechnen.

Eine weitere wichtige Sache zu beachten ist, dass das Konvertieren eines Datums in einen String davon abhängen kann, in welcher Sprache und Region Sie arbeiten. In diesem Fall können Sie das Modul `locale` verwenden, um die richtige Formatierung für das gewünschte Land zu erhalten.

## Siehe auch
- [Python-Datums- und Zeitmodul](https://docs.python.org/3/library/datetime.html)
- [Formatierungsoptionen für `strftime()`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)