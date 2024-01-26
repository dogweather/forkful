---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:13:42.402599-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums bedeutet, dass wir den heutigen Tag gemäß dem Systemkalender ermitteln. Programmierer nutzen das heutige Datum, um Zeitstempel zu erstellen, Berichte zu datieren oder zeitabhängige Funktionen auszuführen.

## So geht's:
```elixir
# Aktuelles Datum in Elixir mit der DateTime-Modul:
datum = Date.utc_today()
IO.puts("Heute ist: #{datum}")

# Beispiel Ausgabe
"2023-04-01"
```

Wenn du zusätzlich die aktuelle Uhrzeit benötigst, kannst du das so machen:
```elixir
# Aktuelles Datum und Uhrzeit:
jetzt = DateTime.utc_now()
IO.puts("Aktuelles Datum und Uhrzeit UTC: #{jetzt}")

# Beispiel Ausgabe:
"2023-04-01 12:34:56Z"
```

## Tiefergehendes
Elixir's `Date` und `DateTime` Module verwenden die Kalenderfunktionen von Erlang und bieten eine präzise Zeitberechnung. Historisch gesehen basiert Elixir auf Erlang's robustem System, das perfekt für zeitkritische Anwendungen ist. Vor der Einführung von `DateTime` in Elixir 1.3 mussten Entwickler externe Bibliotheken verwenden oder manuell Zeit in Elixir berechnen. Alternative Libraries wie "Timex" bieten zusätzliche Funktionen und Formatoptionen, bleiben aber meist für komplexere Szenarien reserviert. Wichtig zu wissen: Elixir behandelt Zeitangaben standardmäßig als UTC, für andere Zeitzonen muss man die Umrechnung selbst durchführen oder eine entsprechende Bibliothek einbinden.

## Siehe auch
- Elixir's offizielle Dokumentation zu `Date`: https://hexdocs.pm/elixir/Date.html
- Elixir's offizielle Dokumentation zu `DateTime`: https://hexdocs.pm/elixir/DateTime.html
- Erlang's Kalender-Dokumentation: https://erlang.org/doc/man/calendar.html
- Die "Timex" Bibliothek für komplexere Zeitberechnungen: https://hex.pm/packages/timex
