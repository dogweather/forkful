---
title:    "Elixir: Ein Datum in einen String umwandeln"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Bei der Programmierung in Elixir kann es manchmal notwendig sein, ein Datum in eine Zeichenfolge (String) umzuwandeln, um es leichter lesen und verarbeiten zu können.

## Wie das geht

Um ein Datum in Elixir in einen String umzuwandeln, gibt es verschiedene Möglichkeiten. Eine einfache Methode ist die Verwendung der Funktion `to_string/1`. Diese Funktion akzeptiert als Argument ein Datum im Elixir-internen Format und gibt einen String zurück, der das Datum im üblichen Format anzeigt. Hier ist ein Beispiel:

```Elixir
date = ~D[2020-12-25]
result = to_string(date)
IO.puts(result)
```

Die Ausgabe dieses Codes wird `2020-12-25` sein, da das Datum im Jahr-Monat-Tag-Format angezeigt wird.

Eine weitere Möglichkeit ist die Verwendung der Funktion `Calendar.Format.format/2`, die es ermöglicht, das Datum in einem benutzerdefinierten Format anzuzeigen. Hier ist ein Beispiel:

```Elixir
date = ~D[2020-12-31]
result = Calendar.Format.format(date, "{1}/{2}/{3}")
IO.puts(result)
```

Die Ausgabe dieses Codes wird `12/31/2020` sein, da das Datum nun im Monat/Tag/Jahr-Format angezeigt wird.

## Tiefer einsteigen

Wenn wir uns die Funktion `to_string/1` genauer ansehen, können wir erkennen, dass sie eine Instanz der `%Elixir.Calendar.Date{}`-Struktur akzeptiert. Dies macht das Datum in Elixir sehr flexibel, da es nicht an ein bestimmtes Format gebunden ist. Wir können auch auf die einzelnen Komponenten des Datums zugreifen, indem wir die Funktionen `Calendar.Date.year/1`, `Calendar.Date.month/1` und `Calendar.Date.day/1` verwenden. Hier ist ein Beispiel:

```Elixir
date = ~D[2021-01-15]
IO.puts("Das Jahr ist #{Calendar.Date.year(date)} und der Monat ist #{Calendar.Date.month(date)}.")
```

Die Ausgabe dieses Codes wird `Das Jahr ist 2021 und der Monat ist 1.` sein.

## Sieh auch

- [Elixir Dokumentation zu `to_string/1`](https://hexdocs.pm/elixir/Calendar.Date.html#to_string/1)
- [Elixir Dokumentation zu `Calendar.Format.format/2`](https://hexdocs.pm/elixir/Calendar.Format.html#format/2)