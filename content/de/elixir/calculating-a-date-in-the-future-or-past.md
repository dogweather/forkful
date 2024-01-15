---
title:                "Berechnen eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Elixir: Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Daten in der Zukunft oder Vergangenheit kann in vielen Programmierprojekten sehr nützlich sein. Zum Beispiel kann es in der Finanzbranche verwendet werden, um zukünftige Zahlungen vorherzusagen oder in der Logistik, um Liefertermine zu planen.

## Wie

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die `Date` Klasse in Elixir verwenden. Hier ist ein Beispielcode, der das Datum von heute ausgibt:

```Elixir
Date.today()
```

Um ein zukünftiges Datum zu berechnen, können wir die Funktion `add` verwenden, die eine positive Anzahl von Tagen als Argument erhält. Zum Beispiel, um das Datum von morgen zu berechnen:

```Elixir
Date.add(Date.today(), 1)
```

Ähnlich können wir ein vergangenes Datum berechnen, indem wir eine negative Anzahl von Tagen als Argument übergeben. Zum Beispiel, um das Datum von gestern zu berechnen:

```Elixir
Date.add(Date.today(), -1)
```

Um ein bestimmtes Datum zu berechnen, können wir auch die Funktion `from_erl!` verwenden, die ein Tuple mit dem Datum als Argument erhält. Zum Beispiel, um den 1. Januar 2020 zu berechnen:

```Elixir
Date.from_erl!({2020, 1, 1})
```

## Deep Dive

Die `Date` Klasse in Elixir verwendet eine interne Repräsentation von Datumswerten als Tuple mit drei Elementen: Jahr, Monat und Tag. Dadurch kann sie effizient und präzise Berechnungen durchführen. Es ist auch wichtig zu beachten, dass die Funktionen `add` und `from_erl!` immer neue Datumswerte zurückgeben und das ursprüngliche Datum nicht ändern.

## Siehe Auch

- [ExCalendar: Einfache Datum- und Zeitmanipulation in Elixir](https://github.com/lau/ex_calendar)
- [Erlang Date and Time Functions Dokumentation](http://erlang.org/doc/man/calendar.html)
- [Elixir Date Modul Dokumentation](https://hexdocs.pm/elixir/Date.html)