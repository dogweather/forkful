---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Mit Elixir das aktuelle Datum abrufen

## Was & Warum?

Das Abrufen des aktuellen Datums ist eine Technik, die oft in Software- und Webentwicklung eingesetzt wird, um heutige Daten in Echtzeit zu überwachen oder zu protokollieren. Dies ist in einer Vielzahl von Anwendungsfällen nützlich, wie z.B. Zeitstempel auf Datenbanken, Verfolgen von Aktivitäten und vieles mehr.

## Wie macht man das:

In Elixir können wir die in eingebaute Funktion `Date.utc_today()` verwenden, um das heutige Datum in UTC zu erhalten. 

```elixir
heute = Date.utc_today()
IO.puts(to_string(heute))

```
Die Ausgabe wäre z.B.:

```elixir
"2022-01-26"
```

## Vertiefung

Historisch gesehen war das Abrufen des Datums immer relevant, seit Menschen begannen, Zeit zu verfolgen. In der Welt der Programmierung kann das Elixir-System das aktuelle Datum vom Betriebssystem abrufen, auf dem es ausgeführt wird.

Bei der Auswahl von Alternativen muss man die Zeitzone beachten. `Date.utc_today()` gibt UTC-Datum zurück. Für das lokale Datum verwenden Sie `Date.local_today()`.

Die Implementierungsdetails hängen stark von der verwendeten Elixir-Version ab. Da Elixir auf der Erlang-Laufzeit aufbaut, hängt die Datumsermittlung letztlich von der Funktionsweise des Erlang-Systems ab.

## Siehe auch 

Weitere Informationen und hilfreiche Beispiele finden Sie in der offiziellen Elixir-Dokumentation zu `Date` unter [https://hexdocs.pm/elixir/Date.html](https://hexdocs.pm/elixir/Date.html). Es lohnt sich, einen Blick darauf zu werfen. Und natürlich ist Google oder StackOverflow immer eine gute Anlaufstelle, um mehr Informationen oder Beispiellösungen zu finden.