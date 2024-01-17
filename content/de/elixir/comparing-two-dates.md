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

## Was & Warum?

Beim Vergleichen von zwei Daten geht es darum, festzustellen, ob zwei Datumsangaben gleich, größer oder kleiner sind als einander. Programmierer verwenden dies, um Daten in einer bestimmten Reihenfolge zu sortieren oder um zu überprüfen, ob ein Datum in einem bestimmten Zeitraum liegt.

## Wie sollte man das machen:

Um zwei Daten in Elixir zu vergleichen, können wir die built-in Funktion `Date.compare/2` verwenden. Es akzeptiert zwei Daten als Argumente und gibt eine Zahl zurück, die den Vergleich zwischen den beiden Daten angibt. Eine positive Zahl bedeutet, dass das erste Datum größer ist, eine negative Zahl bedeutet, dass das zweite Datum größer ist und eine Null bedeutet, dass beide Daten gleich sind.

```Elixir
iex> Date.compare(~D[2021-01-01], ~D[2020-01-01])
1
iex> Date.compare(~D[2019-01-01], ~D[2020-01-01])
-1
iex> Date.compare(~D[2020-05-15], ~D[2020-05-15])
0
```

## Tieferer Einblick:

Das Vergleichen von Daten ist ein wichtiger Teil der Programmierung, da es uns hilft, Daten in einer sinnvollen Reihenfolge zu organisieren und zu verarbeiten. Vor Elixir gab es bereits zahlreiche Möglichkeiten zur Manipulation von Daten und zum Vergleichen von Datumsangaben, aber Elixir hat es geschafft, einen einfachen und effizienten Ansatz für diese Aufgabe zu bieten.

Eine Alternative zur `Date.compare/2` Funktion ist die Verwendung des `:calendar.compare/2` Moduls aus der Standardbibliothek. Dieses Modul bietet ähnliche Funktionen wie `Date.compare/2`, ist jedoch etwas allgemeiner, da es nicht nur mit Daten, sondern auch mit anderen Datentypen wie Zeit und Datumszeit arbeiten kann.

## Siehe auch:

- Offizielle Elixir Dokumentation für die `Date` Module: https://hexdocs.pm/elixir/Date.html
- `:calendar.compare/2` Modul: https://hexdocs.pm/elixir/Calendar.html#compare/2