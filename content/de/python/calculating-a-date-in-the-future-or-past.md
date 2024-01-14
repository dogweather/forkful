---
title:    "Python: Ein Datum in der Zukunft oder Vergangenheit berechnen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit kann hilfreich sein, um beispielsweise Termine zu planen oder Daten für bestimmte Analysen zu sammeln.

## Wie geht man vor?

Die Berechnung eines Datums in der Zukunft oder Vergangenheit in Python ist ganz einfach. Zunächst müssen wir das `datetime` Modul importieren:

```Python
import datetime
```

Als Nächstes können wir ein bestimmtes Datum als `datetime` Objekt erstellen und der Variablen `datum` zuweisen:

```Python
datum = datetime.datetime(2021, 10, 15)
```

Um nun ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die `timedelta` Funktion verwenden. Diese Funktion erwartet als Argument eine Anzahl an Tagen, Wochen, Monaten oder Jahren. Wir können auch verschiedene Kombinationen von Zeiteinheiten angeben.

```Python
zukuenftiges_datum = datum + datetime.timedelta(days=30)
vergangenes_datum = datum - datetime.timedelta(weeks=2)
```

Um das Ergebnis auszugeben, können wir einfach die `print` Funktion verwenden:

```Python
print("Das zukünftige Datum ist:", zukuenftiges_datum)
print("Das vergangene Datum war:", vergangenes_datum)
```

Das resultierende Output sieht folgendermaßen aus:

```
Das zukünftige Datum ist: 2021-11-14 00:00:00
Das vergangene Datum war: 2021-09-15 00:00:00
```

## Tiefergehende Informationen

Das `datetime` Modul bietet noch weitere nützliche Funktionen für die Arbeit mit Datumsangaben. So können wir beispielsweise das aktuelle Datum und die aktuelle Uhrzeit mit der Funktion `now` abrufen:

```Python
aktuelles_datum = datetime.datetime.now()
```

Außerdem gibt es die `strftime` Funktion, mit der wir ein `datetime` Objekt in ein bestimmtes Datumsformat umwandeln können. Diese Funktion erwartet als Argument einen sogenannten Formatstring, der die gewünschte Darstellung des Datums definiert. Einige häufig verwendete Codes sind:

- `%d` für Tag (zweistellig)
- `%m` für Monat (zweistellig)
- `%Y` für Jahr (vierstellig)
- `%H` für Stunde (zweistellig, 24-Stunden-Format)
- `%M` für Minute (zweistellig)
- `%S` für Sekunde (zweistellig)

Ein Beispiel für die Verwendung dieser Funktion wäre:

```Python
print("Heute ist der", aktuelles_datum.strftime('%d.%m.%Y'))
```

Das Ergebnis wäre dann beispielsweise `Heute ist der 27.10.2021`.

Weitere Informationen und Möglichkeiten des `datetime` Moduls können in der offiziellen Dokumentation von Python gefunden werden.

## Siehe auch

- [Offizielle Dokumentation des `datetime` Moduls](https://docs.python.org/de/3/library/datetime.html)
- [Tutorial zur Arbeit mit Datumsangaben in Python](https://www.python-kurs.eu/python3_formatierte_ausgabe.php) (auf Deutsch)
- [Beispiele zur Verwendung von `timedelta`](https://www.geeksforgeeks.org/python-datetime-timedelta-function/) (auf Englisch)