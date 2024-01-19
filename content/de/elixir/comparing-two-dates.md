---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Vergleichen zweier Daten ist das Erkennen des Unterschieds zwischen zwei Zeitpunkten. Programmierer tun dies, um einen Zeitrahmen für Ereignisse, Datenanalysen, Zeitstempel und andere zeitbezogene Funktionen festzulegen.

## Wie macht man das?

```Elixir
# Datumsangaben erstellen
datum1 = Date.new(2020, 12, 15)
datum2 = Date.new(2019, 12, 15)

# Vergleiche
Date.compare(datum1, datum2)
```

Die Ausgabe hier wäre:

```elixir
{:gt, %{sign: 1, day: 366, second: 31622400, microsecond: {0, 0}}}
```

Das zeigt, dass `datum1` größer als `datum2` ist (d.h., es liegt später). 

## Tiefer Tauchen

1. Historische Kontext: Elixir, eine im Jahr 2012 von José Valim erstellte Programmiersprache, nutzt den Erlang VM, um seine Funktionen zu erweitern und den Umgang mit Terminkomplexitäten zu erleichtern. 
   
2. Alternativen: Andere Sprachen, wie Python oder Java, haben ebenfalls eingebaute Funktionen für Datumsvergleiche, obwohl die Syntax variiert.
   
3. Implementierungsdetails: Elixir verwendet eine Struktur namens `DateTime` zur Verwaltung von Daten und Zeiten. Bei der Komparation von zwei Daten mit `Date.compare()` gibt die Funktion ein Tupel zurück, das einen Vergleichsoperator (:lt für kleiner, :gt für größer, :eq für gleich) und ein Map mit der Differenz in Tagen, Sekunden und Mikrosekunden enthält.

## Siehe Auch

- Offizielle Elixir-Dokumentation: [Date.compare/2](https://hexdocs.pm/elixir/Date.html#compare/2)
- Elixir-Schulungsmaterialien: [Elixir School](https://elixirschool.com/de/)