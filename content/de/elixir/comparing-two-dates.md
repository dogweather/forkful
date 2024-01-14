---
title:                "Elixir: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Vergleichen von Datumswerten ist ein häufiger Bestandteil vieler Programmieraufgaben. Es ermöglicht uns, zu überprüfen, ob ein bestimmtes Datum in der Zukunft liegt, zu prüfen, ob zwei Datumsangaben identisch sind oder um zu bestimmen, welches Datum früher oder später liegt. In dieser Elixir-Blog-Post werden wir lernen, wie man effektiv zwei Datumswerte vergleicht und welche wichtigen Dinge dabei zu beachten sind.

## Wie gehts's

Der Vergleich von zwei Datumswerten in Elixir ist relativ einfach und kann mithilfe des `Date.compare/2`-Funktion durchgeführt werden. Wir können dies anhand eines einfachen Beispiels demonstrieren:

```Elixir
lower_date = Date.new(2021, 7, 1)
higher_date = Date.new(2021, 8, 1)

comparison = Date.compare(lower_date, higher_date)

IO.puts "Der untere Termin ist " <> (if comparison == -1, do: "früher als", else: "später als") <> " der obere Termin."
```

In diesem Beispiel definieren wir zwei Datumswerte und verwenden dann die `Date.compare/2`-Funktion, um sie zu vergleichen. Der Rückgabewert ist entweder `-1`, `0` oder `1`, je nachdem, ob das erste Datum früher, gleich oder später als das zweite Datum liegt. Wir nutzen das Ergebnis, um eine entsprechende Aussage auszugeben.

Ein weiteres nützliches Beispiel ist der Vergleich von aktuellen Datumswerten mit einem bestimmten Datum in der Zukunft. Dies kann mithilfe des `Date.future?/2`-Funktion durchgeführt werden:

```Elixir
current_date = Date.utc_today()
future_date = Date.new(2022, 1, 1)

is_future = Date.future?(current_date, future_date)

if is_future do
  IO.puts "Das aktuelle Datum liegt in der Zukunft."
else
  IO.puts "Das aktuelle Datum liegt in der Vergangenheit."
end
```

Hier verwenden wir die `Date.utc_today/0`-Funktion, um das aktuelle Datum abzurufen, und die `Date.future?/2`-Funktion, um zu überprüfen, ob das aktuelle Datum später als das zukünftige Datum liegt.

## Tiefer Einblick

Obwohl der Vergleich von Datumswerten in Elixir einfach scheint, gibt es einige wichtige Dinge zu beachten. Zum Beispiel können Datumswerte in verschiedenen Zeitzonen sehr unterschiedlich interpretiert werden, wodurch es zu unerwarteten Ergebnissen beim Vergleichen kommen kann. Es ist daher wichtig, sicherzustellen, dass alle Datumswerte in der gleichen Zeitzone sind, bevor sie verglichen werden.

Ein weiteres wichtiges Konzept beim Vergleich von Datumswerten ist die Verwendung von Zeitstempeln. Das Vergleichen von Zeitstempeln ist effizienter und genauer als das Vergleichen von Datumswerten. Elixir bietet die `DateTime.to_unix/1`-Funktion, um ein Datum in einen Zeitstempel umzuwandeln, der dann problemlos mit anderen Zeitstempeln verglichen werden kann.

## Siehe auch

- Elixir Date-Modul Dokumentation: https://hexdocs.pm/elixir/Date.html
- Wikipedia-Artikel über Datumsvergleiche: https://de.wikipedia.org/wiki/Datumsvergleich
- Vergleich von Zeitdifferenzen in Elixir: https://dev.to/andrewgregory/working-with-time-in-elixir-2i0o