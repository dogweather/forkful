---
title:    "Elixir: Einen zukünftigen oder vergangenen Datum berechnen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Szenarien, in denen das Berechnen eines Datums in der Zukunft oder Vergangenheit hilfreich sein kann, z.B. beim Erstellen von Terminplanern oder bei der Erfassung von Daten in einer Datenbank. Mit Elixir und seinen eingebauten Funktionen ist es einfach, solche Berechnungen durchzuführen.

## Wie geht das?
Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, kann die Funktion `Date.add` verwendet werden. Hier ist ein Beispiel, um das Datum in 30 Tagen zu berechnen:

```Elixir
today = Date.today()                # Zurückgegebenes Datum: 2020-09-10
future_date = Date.add(today, 30)   # Zurückgegebenes Datum: 2020-10-10
```

Um das Datum in der Vergangenheit zu berechnen, muss lediglich ein negativer Wert für die Anzahl der Tage übergeben werden:

```Elixir
today = Date.today()                     # Zurückgegebenes Datum: 2020-09-10
past_date = Date.add(today, -90)         # Zurückgegebenes Datum: 2020-06-12
```

Die berechneten Daten können dann weiter verarbeitet oder in eine Datenbank gespeichert werden.

## Tiefergehende Informationen
Elixir bietet auch eine Reihe anderer Funktionen für die Arbeit mit Datum und Uhrzeit, wie z.B. `Date.subtract`, um eine bestimmte Anzahl von Tagen zu subtrahieren, `Date.compare`, um mit Datumswerten zu vergleichen, und `Date.utc_now`, um die aktuelle UTC-Uhrzeit abzurufen. Weitere Informationen zu diesen Funktionen und deren Verwendung finden Sie in der offiziellen Elixir-Dokumentation.

## Siehe auch
- [Elixir-Dokumentation zu Datums- und Uhrzeitfunktionen](https://hexdocs.pm/elixir/Date.html)
- [Tutorial zum Umgang mit Datums- und Uhrzeitfunktionen in Elixir](https://elixirschool.com/de/lessons/advanced/date-time/)