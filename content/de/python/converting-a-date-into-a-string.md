---
title:                "Python: Umwandlung eines Datums in einen String"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Konvertierung von Datum in einen String kann hilfreich sein, um das Datum in einem für den Benutzer leicht lesbaren Format anzuzeigen oder um es zu speichern und später wieder zu verwenden.

## Wie man es macht

Um ein Datum in einen String umzuwandeln, können Sie die `strftime()` Funktion verwenden. Hier ist ein Beispielcode, der ein heutiges Datum in verschiedenen Formaten ausgibt:

```Python
from datetime import date

heute = date.today().strftime("%d.%m.%Y")
print("Heute ist der " + heute) # gibt "Heute ist der 07.07.2020" aus

morgen = date.today() + timedelta(days=1)
print(morgen.strftime("%A, %d.%m.%Y")) # gibt "Dienstag, 08.07.2020" aus
```

Im obigen Code wird das heutige Datum in einem benutzerdefinierten Format mithilfe der `strftime()` Funktion ausgegeben. Beachten Sie, dass die Formatierungsoptionen je nach gewünschtem Ausgabeformat variieren können.

## Tiefere Einblicke

Die `strftime()` Funktion basiert auf der `strftime()`-Methode in C, weshalb die Formatierungsoptionen der Standard-C-Bibliothek entsprechen. Diese Optionen können je nach Betriebssystem variieren. Mit der `strptime()` Funktion können Sie das Gegenteil erreichen und einen String in ein Datum konvertieren.

Schauen wir uns ein Beispiel für die Verwendung der `strptime()` Funktion an:

```Python
import datetime as dt

strings = ["22.11.2000", "12/03/1995", "05-07-2019"]
for s in strings:
    date = dt.datetime.strptime(s, "%d.%m.%Y") 
    # erwartet das Datum im Format TT.MM.JJJJ und wandelt es in einen datetime-Objekt um
    print(date.strftime("%Y")) # gibt nur das Jahr aus

# gibt 2000, 1995, 2019 aus
```

Mit dieser Funktion können Sie also einen String in ein Datum in einem bestimmten Format umwandeln.

## Siehe auch

- [Dokumentation der `strftime()` Funktion](https://docs.python.org/de/3/library/datetime.html#strftime-and-strptime-behavior)
- [Weitere Informationen über Datum und Zeit in Python](https://www.w3schools.com/python/python_datetime.asp)