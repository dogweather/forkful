---
title:                "Vergleich von zwei Daten"
html_title:           "Elixir: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Vertraust du darauf, dass die aktuelle Zeit korrekt angezeigt wird? Oder bist du schon einmal auf falsche oder ungewöhnliche Datumseinstellungen auf deinem Computer oder Gerät gestoßen? In diesem Artikel werden wir uns damit beschäftigen, wie du in Elixir zwei Daten vergleichen kannst, um sicherzustellen, dass sie korrekt sortiert und verglichen werden.

## Wie geht das?

Um zwei Daten miteinander zu vergleichen, müssen wir sie zuerst in den richtigen Datentyp umwandeln. Dafür gibt es die Funktion `NaiveDateTime.from_iso8601`, die eine Zeichenkette im ISO8601-Format in ein Datum und eine Uhrzeit umwandelt. Zum Beispiel:

```Elixir
  NaiveDateTime.from_iso8601("2021-01-05T10:30:00")
```

Wir können auch die Funktion `DateTime.from_iso8601` verwenden, um ein Datum mit Zeitzoneninformationen zu erhalten. Zum Beispiel:

```Elixir
  DateTime.from_iso8601("2021-01-05T10:30:00+0100")
```

Sobald wir unsere Daten in den richtigen Datentyp umgewandelt haben, können wir sie einfach mit dem Vergleichsoperator `>` oder `<` vergleichen. Zum Beispiel:

```Elixir
  date1 = NaiveDateTime.from_iso8601("2021-01-05T10:30:00")
  date2 = NaiveDateTime.from_iso8601("2021-01-06T10:30:00")

  date1 < date2 # true
```

## Tiefer eintauchen

Wusstest du, dass hinter den Kulissen in Elixir alle Daten und Zeiten als Millisekunden seit dem 1. Januar 1970 gespeichert werden? Das bedeutet, dass das Vergleichen von zwei Daten letztendlich auf den Vergleich von Zahlen reduziert wird. Elixir ist auch in der Lage, mit Schaltjahren und Schaltsekunden umzugehen, was die Vergleichsfunktionen noch zuverlässiger macht.

## Siehe auch

- [Elixir-Dokumentation zur Funktion `NaiveDateTime.from_iso8601`](https://hexdocs.pm/elixir/NaiveDateTime.html#from_iso8601/1)
- [Elixir-Dokumentation zur Funktion `DateTime.from_iso8601`](https://hexdocs.pm/elixir/DateTime.html#from_iso8601/1)