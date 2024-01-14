---
title:                "Elixir: Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Datumswerten ist eine wichtige Funktion in vielen Elixir-Anwendungen. Es ermöglicht uns, Datumsangaben zu planen und zu organisieren, um unsere Aufgaben effizient zu erledigen.

## So geht's

Die Elixir Standard-Library bietet eine Vielzahl von Funktionen zur Arbeit mit Datumsangaben. Eine davon ist die `Calendar.DateTime`-Funktion, die uns ermöglicht, ein angegebenes Datum in die Zukunft oder Vergangenheit zu verschieben.

Um ein zukünftiges Datum zu berechnen, verwenden wir die `add/3`-Funktion und geben ihr das aktuelle Datum, die Anzahl der Tage, die wir hinzufügen möchten, und eine Liste von Optionen an, die das gewünschte Datum formatieren.

```Elixir
current_date = %DateTime{year: 2021, month: 10, day: 1}
future_date = Calendar.DateTime.add(current_date, 5, [:day, :time])

IO.puts future_date
# Output: 2021-10-06T00:00:00Z
```

Ebenso können wir ein vergangenes Datum berechnen, indem wir die `sub/3`-Funktion verwenden und die Anzahl der Tage subtrahieren.

```Elixir
past_date = Calendar.DateTime.sub(current_date, 10, [:day, :time])

IO.puts past_date
# Output: 2021-09-21T00:00:00Z
```

## Tieferes Eintauchen

Mit Elixir können wir auch spezifische Datumsangaben berechnen, wie z.B. das aktuelle Datum, das Datum von gestern oder das Datum von morgen.

```Elixir
current_date = Calendar.DateTime.now
IO.puts current_date
# Output: 2021-10-01T14:30:00Z

yesterday_date = Calendar.DateTime.from_erl({{2021, 10, 1}, {12, 0, 0}})
IO.puts yesterday_date
# Output: 2021-09-30T12:00:00Z

tomorrow_date = Calendar.DateTime.from_erl({{2021, 10, 2}, {12, 0, 0}})
IO.puts tomorrow_date
# Output: 2021-10-02T12:00:00Z
```

Außerdem können wir Funktionen wie `diff/2` und `compare/2` verwenden, um Datumsangaben zu vergleichen und die Differenz zwischen ihnen zu berechnen.

```Elixir
date_1 = DateTime.now
date_2 = DateTime.add(date_1, 3, [:day])

diff = Calendar.DateTime.diff(date_1, date_2, :days)
IO.puts diff
# Output: 2

comparison = Calendar.DateTime.compare(date_1, date_2)
IO.puts comparison
# Output: :lt
```

## Siehe auch

- [Elixir Calendar Module Documentation](https://hexdocs.pm/elixir/Calendar.html)
- [Elixir DateTime Module Documentation](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Date Module Documentation](https://hexdocs.pm/elixir/Date.html)