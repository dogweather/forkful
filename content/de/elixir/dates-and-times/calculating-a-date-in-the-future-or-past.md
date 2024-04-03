---
date: 2024-01-20 17:28:32.420577-07:00
description: 'How to: Elixir macht Datumskalkulation einfach mit dem `Date` Modul.
  Hier ist, wie du es machst.'
lastmod: '2024-03-13T22:44:53.548833-06:00'
model: gpt-4-1106-preview
summary: Elixir macht Datumskalkulation einfach mit dem `Date` Modul.
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
weight: 26
---

## How to:
Elixir macht Datumskalkulation einfach mit dem `Date` Modul. Hier ist, wie du es machst:

```elixir
# Aktuelles Datum
aktuelles_datum = Date.utc_today()

# Datum in 10 Tagen
zukunft_datum = Date.add(aktuelles_datum, 10)
IO.puts(zukunft_datum)

# Datum vor 5 Tagen
vergangenheit_datum = Date.add(aktuelles_datum, -5)
IO.puts(vergangenheit_datum)
```
Beispiel Output:
```
2024-04-10
2024-03-26
```

Benutze `NaiveDateTime.add/4`, wenn du auch Zeit berücksichtigen möchtest:

```elixir
aktuelle_zeit = NaiveDateTime.utc_now()

# 1000 Sekunden in die Zukunft
zukunft_zeit = NaiveDateTime.add(aktuelle_zeit, 1_000)
IO.puts(zukunft_zeit)

# 3600 Sekunden (1 Stunde) in der Vergangenheit
vergangenheit_zeit = NaiveDateTime.add(aktuelle_zeit, -3_600)
IO.puts(vergangenheit_zeit)
```

## Deep Dive:
Die Datumsberechnung gab es schon in frühen Programmiersprachen. In Elixir macht es das eingebaute `Date` Modul leicht. Für komplexere Berechnungen gibt es Third-Party Libraries wie `Timex`, die zusätzliche Funktionalitäten bieten. Wichtig ist die Zeitzone – `Date` rechnet ohne Zeitzonen, während `DateTime` und `NaiveDateTime` UTC verwenden. Zeitberechnung ist wegen Schaltsekunden und Zeitumstellungen komplex. Deshalb sollten Operationen auf Daten und Zeiten vermieden werden, die ohne den Kontext der Zeitzone stattfinden.

## See Also:
- [Elixir's Date module](https://hexdocs.pm/elixir/Date.html)
- [Elixir's NaiveDateTime module](https://hexdocs.pm/elixir/NaiveDateTime.html)
