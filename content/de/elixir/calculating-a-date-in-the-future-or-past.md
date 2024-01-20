---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
html_title:           "Elixir: Berechnung eines zukünftigen oder vergangenen Datums"
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Berechnung eines zukünftigen oder vergangenen Datums ist eine gängige Programmieroperation. Diese Operation ist nützlich, um Berechnungen wie Fristen, Verzögerungen oder Ereignisdaten durchzuführen.

## So Geht's:

In Elixir gibt es eine wunderbare Bibliothek namens `Timex`, die Ihre Vorgaben erfüllen kann. 

Zuerst müssen Sie es zu Ihrer App hinzufügen:
```Elixir
def deps do
  [
    {:timex, "~> 3.6"}
  ]
end
```

Für die Berechnung eines zukünftigen Datums könnten Sie das folgende tun:
```Elixir
iex> duration = Timex.Duration.from_days(3)
iex> future_date = Timex.shift(Timex.now(), days: duration)
iex> Timex.to_date(future_date)
~D[2021-12-19]
```

Und für ein Datum in der Vergangenheit:
```Elixir
iex> duration = Timex.Duration.from_days(3)
iex> past_date = Timex.shift(Timex.now(), days: -duration)
iex> Timex.to_date(past_date)
~D[2021-12-13]
```

## Tief Tauchen:

Historisch betrachtet, wurde dies schon immer anders in den verschiedenen Programmiersprachen gemacht. In Elixir haben Sie das Glück, dass mit `Erlang/OTP 21` und höher natives Zeit-Tracking eingeführt wurde, was den Umgang mit Datum und Zeit erheblich vereinfacht hat.

Alternativen zu `Timex` sind `Calendar` und `Ecto.DateTime`. `Timex` wird jedoch allgemein als die vollständigste und flexibelste Option angesehen. 

Die Implementierungsdetails für das Berechnen von Daten in der Zukunft oder Vergangenheit sind eigentlich recht einfach, das schwierige Teil ist genau zu definieren, was "3 Tage später" bedeutet. Es könnte 3 * 24 Stunden später bedeuten, es könnte am selben Ort der nächsten Tage bedeuten, abhängig von den Anforderungen.

## Siehe Auch:

- ["NaiveDateTime" in Elixir-Dokumentation](https://hexdocs.pm/elixir/NaiveDateTime.html)
- ["Calendar" in Elixir-Dokumentation](https://hexdocs.pm/calendar/readme.html)