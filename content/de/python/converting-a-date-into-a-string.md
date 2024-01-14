---
title:    "Python: Eine Datum in einen String umwandeln"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Warum

Die Konvertierung von einem Datum in einen String ist ein wichtiger Schritt in der Python Programmierung, da sie es ermöglicht, Daten in menschenlesbarer Form darzustellen. Dies ist besonders nützlich, wenn man Daten in einer Datei speichern oder sie für die Ausgabe auf der Konsole formatieren möchte.

## Wie geht das?

Um ein Datum in einen String umzuwandeln, gibt es in Python verschiedene Methoden, je nach dem gewünschten Format. Hier ist ein Beispiel, wie man das aktuelle Datum in dem Format "Tag, Monat Jahr" ausgeben kann:

```Python
import datetime

current_date = datetime.datetime.now()
date_string = current_date.strftime("%d. %B %Y")

print(date_string) # output: 22. April 2021
```

Dieses Beispiel verwendet die Methode `strftime()` (String from time) aus dem `datetime` Modul. Mit dem `%d` werden die Tageszahl, mit `%B` der Monatsname und mit `%Y` das Jahr ausgegeben. Es gibt noch viele weitere Parameter, die verwendet werden können, um das Datum in verschiedenen Formaten darzustellen. Eine ausführliche Liste und Erklärung findet man in der offiziellen [Python-Dokumentation](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior).

Es ist auch möglich, das Datum als String zu formatieren, ohne das `datetime` Modul zu verwenden, indem man die `str()` Funktion verwendet. Hierbei ist es jedoch wichtig, dass das Datum bereits als `datetime` Objekt vorliegt, da die `str()` Funktion nur die Darstellung des Objekts ändert und nicht das Objekt selbst konvertiert.

## Tiefergehende Informationen

Das `datetime` Modul bietet noch viele weitere Funktionen und Methoden, die für die Arbeit mit Datumsangaben verwendet werden können. Zum Beispiel gibt es die Möglichkeit, Zeitzonen zu berücksichtigen oder mit Datumsdifferenzen zu arbeiten. Es lohnt sich hier, etwas mehr Zeit zu investieren und sich mit den unterschiedlichen Möglichkeiten auseinanderzusetzen, um das Arbeiten mit Datumsangaben in Python noch effizienter zu gestalten.

Ein weiterer wichtiger Punkt bei der Konvertierung von Datum zu String ist die Lokalisierung. Je nach Spracheinstellung des Systems oder der Anwendung kann es sein, dass die Namen von Wochentagen oder Monaten anders ausgegeben werden. Um hier konsistent zu bleiben, kann das `locale` Modul verwendet werden, um die gewünschte Spracheinstellung anzugeben.

## Siehe auch

- [Python-Dokumentation zu `datetime`](https://docs.python.org/3/library/datetime.html)
- [Weitere Beispiele für die Konvertierung von Datum zu String in Python](https://www.w3schools.com/python/python_datetime.asp)
- [Informationen zur Lokalisierung in Python](https://docs.python.org/3/library/locale.html)