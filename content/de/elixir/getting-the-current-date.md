---
title:                "Elixir: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das aktuelle Datum ist ein wichtiger Bestandteil vieler Programme und Anwendungen. Egal ob es um das Speichern von Zeitstempeln, das Planen von Aufgaben oder das Anzeigen von aktuellen Informationen geht, das Abrufen des aktuellen Datums ist ein wesentlicher Schritt. In diesem Artikel lernen wir, wie man mithilfe von Elixir das aktuelle Datum abrufen kann.

## Wie geht's

Um das aktuelle Datum in Elixir zu erhalten, können wir die Funktion `Date.utc_today/0` verwenden. Diese Funktion gibt das aktuelle Datum in der UTC-Zeitzone als `{:ok, date}`-Tuple zurück.

```Elixir
iex> Date.utc_today()
{:ok, ~D[2020-09-07]}
```

Wir können auch das Modul `DateTime` verwenden, um zusätzliche Informationen wie die aktuelle Uhrzeit zu erhalten. Dazu können wir die Funktion `DateTime.utc_now/0` verwenden, die ein `{:ok, datetime}`-Tuple zurückgibt.

```Elixir
iex> DateTime.utc_now()
{:ok, ~U[2020-09-07 14:30:00.000301Z]}
```

Wir können auch das Format des zurückgegebenen Datums anpassen, indem wir die Funktion `format/2` verwenden. Hier ein Beispiel, wie man das Datum im deutschen Format (TT.MM.JJJJ) anzeigen kann:

```Elixir
iex> {:ok, date} = Date.utc_today()
{:ok, ~D[2020-09-07]}

iex> Date.format(date, "{DD}.{MM}.{YYYY}")
"07.09.2020"
```

In Elixir gibt es auch das Modul `Calendar` mit Funktionen zur Verarbeitung von Datums- und Zeitwerten. Hier ein Beispiel, wie man das aktuelle Datum in verschiedenen Formaten anzeigen kann:

```Elixir
iex> date = Calendar.ISO.day_of_year_date()
%{day: 251, month: 9, year: 2020}

iex> Calendar.ISO.format(date, "{YYYY}-{MM}-{DD}")
"2020-09-07"

iex> Calendar.ISO.format(date, "{MM}/{DD}/{YY}")
"09/07/20"
```

## Tiefentauchen

Obwohl das Abrufen des aktuellen Datums in Elixir recht einfach ist, gibt es einige Dinge, die man beachten sollte. Zum Beispiel kann die UTC-Zeitzone manchmal zu unerwarteten Ergebnissen führen, je nachdem wo man sich geografisch befindet. Auch die Umstellung auf Sommer- und Winterzeit kann zu Problemen führen.

Eine Möglichkeit, diese Probleme zu lösen, ist die Verwendung des `Timex`-Pakets, das zusätzliche Funktionen zum Verarbeiten von Datums- und Zeitwerten bietet. Darüber hinaus bietet die offizielle Dokumentation von Elixir hilfreiche Informationen zu diesem Thema.

## Siehe auch

- Offizielle Elixir Dokumentation, Abschnitt über Datums- und Zeitverarbeitung: https://hexdocs.pm/elixir/Calendar.html
- Timex Dokumentation: https://hexdocs.pm/timex/overview.html
- Artikelsammlung zum Elixir Programmieren: https://beam-blog.de/tag/elixir/