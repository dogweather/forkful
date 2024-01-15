---
title:                "Das aktuelle Datum erhalten"
html_title:           "Elixir: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man sich für das aktuelle Datum interessieren könnte, sei es für die Berechnung von Altersangaben, das Erstellen von aussagekräftigen Dateinamen oder für die Verwendung im Spiel "Guess The Date". In diesem Artikel werden wir uns damit befassen, wie man das aktuelle Datum in Elixir abrufen kann und warum es nützlich sein könnte.

## Anleitung

Das Abrufen des aktuellen Datums in Elixir ist sehr einfach. Man muss lediglich die Funktion `Date.utc_today()` verwenden. Hier ist ein Beispiel:

```Elixir
current_date = Date.utc_today()
IO.puts "Das aktuelle Datum ist #{current_date}"
```

Die Ausgabe wird wie folgt aussehen:

```
Das aktuelle Datum ist 2021-08-03
```

Man kann auch bestimmte Teile des Datums abrufen, wie zum Beispiel den Tag, den Monat oder das Jahr. Hier ist ein Beispiel:

```Elixir
{year, month, day} = Date.utc_today()
IO.puts "Das heutige Datum ist #{day}.#{month}.#{year}"
```

Die Ausgabe wird wie folgt aussehen:

```
Das heutige Datum ist 03.08.2021
```

## Tiefere Einblicke

Wenn man genau hinsieht, wird man merken, dass das aktuelle Datum eigentlich ein Tupel der Form `{year, month, day}` ist und nicht einfach ein String. Dies hat den Vorteil, dass man damit weiterhin rechnen oder Vergleiche durchführen kann. Zum Beispiel kann man überprüfen, ob ein Datum in der Zukunft liegt:

```Elixir
future_date = Date.from_gregorian({2022, 12, 31})
{year, month, day} = Date.utc_today()

if year > future_date.year do
  IO.puts "Das heutige Datum liegt in der Zukunft!"
else
  IO.puts "Das heutige Datum liegt in der Vergangenheit!"
end
```

Die Ausgabe wird je nach aktuellem Datum und `future_date` variieren, aber hier ist ein Beispiel:

```
Das heutige Datum liegt in der Vergangenheit!
```

## Siehe auch

- Offizielle Elixir Dokumentation zu [Dates](https://hexdocs.pm/elixir/DateTime.html)
- Ein ausführliches Tutorial zum Thema [Datum und Zeit in Elixir](https://www.mehdidc.com/posts/date-and-time-in-elixir/)