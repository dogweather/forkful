---
title:                "Elixir: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Warum

Das Berechnen von zukünftigen oder vergangenen Daten ist ein wichtiger Teil der Elixir Programmierung. Es kann Ihnen dabei helfen, bestimmte Abläufe oder Ereignisse in Ihrem Code zu planen und zu steuern. Außerdem ist es eine gute Übung, um Ihr Verständnis von Elixir-Funktionen und Modulen zu vertiefen.

# Wie geht man vor

Um das Berechnen von Datumsangaben in Elixir zu verstehen, müssen wir uns mit dem `Calendar`-Modul vertraut machen. Dieses Modul bietet eine Vielzahl von Funktionen, um mit Daten und Zeiten zu arbeiten. Schauen wir uns zunächst an, wie man ein spezifisches Datum in der Zukunft oder Vergangenheit berechnet.

```elixir
# Berechnen eines Datums, das 7 Tage in der Zukunft liegt
iex> Calendar.utc_today() |> Calendar.add(7, :days)
{:ok, ~N[2021-07-08 00:00:00]}

# Berechnen eines Datums, das 2 Monate in der Vergangenheit liegt
iex> Calendar.utc_today() |> Calendar.add(-2, :months)
{:ok, ~N[2021-04-08 00:00:00]}
```

Wie Sie sehen können, wird das `Calendar`-Modul verwendet, um das heutige Datum zu holen und dann die Funktion `add` aufzurufen, um eine bestimmte Anzahl von Tagen oder Monaten hinzuzufügen oder zu subtrahieren. Das Ergebnis wird als `{:ok, date}` Tupel zurückgegeben. Sie können auch die Funktion `convert` verwenden, um das Datum in ein benutzerfreundlicheres Format zu konvertieren, z.B. mit `~D` für eine Datumsangabe oder `~T` für eine Zeitangabe.

# Tiefere Einblicke

Neben dem einfachen Berechnen von Datumsangaben bietet das `Calendar`-Modul auch verschiedene Funktionen für die Arbeit mit Datums- und Zeitangaben. So können Sie z.B. zwei Datumsangaben vergleichen oder die Differenz zwischen zwei Zeiten berechnen. Das Modul bietet außerdem Unterstützung für verschiedene Zeitzonen und Kalendersysteme. Informieren Sie sich in der offiziellen Dokumentation oder in der Elixir-Community über weitere Möglichkeiten und Funktionen des `Calendar`-Moduls.

# Siehe auch

- [Offizielle Elixir-Dokumentation zum `Calendar`-Modul] (https://hexdocs.pm/elixir/Calendar.html)
- [Blog-Post von Elixir-ninjas über das `Calendar`-Modul] (https://blog.elixir-ninjas.com/how-to-work-with-dates-and-times-in-elixir-43ba88c2b776)
- [Elixir Forum Thread über die Handhabung von Datumsangaben] (https://elixirforum.com/t/how-do-i-work-with-dates-in-elixir/12005/3)