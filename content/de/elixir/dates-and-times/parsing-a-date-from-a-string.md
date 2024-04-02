---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 01:59:23.503759-07:00
description: "Das Parsen eines Datums aus einem String bedeutet, Text wie \u201E2023-04-05\u201C\
  \ zu nehmen und ihn in ein Datumformat umzuwandeln, das Ihr Programm verstehen und\u2026"
lastmod: '2024-03-13T22:44:53.543841-06:00'
model: gpt-4-0125-preview
summary: "Das Parsen eines Datums aus einem String bedeutet, Text wie \u201E2023-04-05\u201C\
  \ zu nehmen und ihn in ein Datumformat umzuwandeln, das Ihr Programm verstehen und\u2026"
title: Eine Datumsauswertung aus einem String
weight: 30
---

## Was & Warum?

Das Parsen eines Datums aus einem String bedeutet, Text wie „2023-04-05“ zu nehmen und ihn in ein Datumformat umzuwandeln, das Ihr Programm verstehen und mit dem es arbeiten kann. Programmierer tun dies, weil Daten in vielen Formaten vorliegen und sie Konsistenz benötigen, um sie ordnungsgemäß vergleichen, sortieren oder speichern zu können.

## Wie:

In Elixir können Sie Daten mit dem Modul `Date` parsen. So verwandeln Sie einen String in ein Datum:

```elixir
date_string = "2023-04-05"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)
```

Beispielausgabe:

```elixir
~D[2023-04-05]
```

Um verschiedene Formate zu behandeln, können Sie die Bibliothek `Timex` verwenden:

```elixir
{:ok, datetime} = Timex.parse("05-04-2023", "{D}-{0M}-{YYYY}")
IO.inspect(datetime)
```

Beispielausgabe:

```elixir
#DateTime<2023-04-05 00:00:00Z>
```

## Vertiefung

Die Funktion `Date.from_iso8601/1` ist Teil der Standardbibliothek von Elixir und wurde eingeführt, um das einfache Parsen des ISO8601-Datumsstandards – einem gängigen Datumsformat – zu gewährleisten. Aber das Leben ist nicht so einfach; Daten kommen in Unmengen von Formaten vor. Dort kommt `Timex`, eine Drittanbieter-Bibliothek von Elixir, ins Spiel. Sie ist reicher an Funktionen als die eingebauten Elixir-Datumsfunktionen und hilft beim Umgang mit einer Vielzahl von Datumsformaten.

Elixir selbst ist unveränderlich, was bedeutet, dass geparste Daten keine Ausnahme sind; sie können nicht verändert werden, sobald sie erstellt wurden. Diese Eigenschaft geht auf die funktionale Programmierwurzeln von Elixir zurück und garantiert Vorhersagbarkeit und erleichtertes Debugging.

Historisch gesehen war das Parsen von Daten aufgrund der unterschiedlichen Standards schwierig. Doch mit Bibliotheken wie `Timex` und Sprachfunktionen in Elixir wird die Komplexität abstrahiert und das Leben eines Entwicklers ein Stück einfacher gemacht.

## Siehe auch

- [Elixir Date](https://hexdocs.pm/elixir/Date.html)
- [Timex Dokumentation](https://hexdocs.pm/timex/Timex.html)
- [ISO8601-Standard](https://www.iso.org/iso-8601-date-and-time-format.html)
