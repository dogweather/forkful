---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:35:30.669724-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Datum-Parsing wandelt Zeichenketten in Datumsformate um. Programmierer nutzen es, um Daten zu vergleichen, zu speichern oder Zeitabläufe zu manipulieren.

## How to:
Hier ein einfaches Beispiel in Elixir:

```elixir
{:ok, date} = Date.from_iso8601("2023-04-01")
IO.inspect(date)
```

Ausgabe:

```elixir
~D[2023-04-01]
```

Um verschiedene Formate zu parsen, benutzen wir die Timex-Bibliothek:

```elixir
{:ok, dt} = Timex.parse("01.04.2023", "{D}.{M}.{YYYY}", :strftime)
IO.inspect(dt)
```

Ausgabe:

```elixir
~N[2023-04-01T00:00:00]
```

## Deep Dive
Elixir nutzt Erlang/OTP-Datumsfunktionen, entwickelt für zuverlässige verteilte Systeme seit den 1980er Jahren. Alternativen wie die Timex-Bibliothek bieten mehr Funktionalitäten, wie Zeitzone- oder Format-Handling. Parsing funktioniert mittels Pattern-Matching und Transformation der Eingabe in das native Elixir-Date-Datatype.

In der Praxis muss man auf verschiedene Probleme achten, z.B. Schaltjahre oder unterschiedliche Kalenderformate. Komplexere Datumsmanipulationen können schnell unübersichtlich werden, deshalb ist es wichtig, robuste Libraries wie Timex zu verwenden und gut über internationale Standards und Zeitzone-Eigenheiten Bescheid zu wissen.

## See Also
- Elixir Date Dokumentation: [https://hexdocs.pm/elixir/Date.html](https://hexdocs.pm/elixir/Date.html)
- Timex GitHub Repo: [https://github.com/bitwalker/timex](https://github.com/bitwalker/timex)
- Erlang Zeitzonenbibliothek (tzdata): [https://github.com/lau/tzdata](https://github.com/lau/tzdata)
- World Calendar Datenbank (IANA Time Zone Database): [https://www.iana.org/time-zones](https://www.iana.org/time-zones)
