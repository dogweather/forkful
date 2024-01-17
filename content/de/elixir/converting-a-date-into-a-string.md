---
title:                "Eine Datumsumwandlung in einen String."
html_title:           "Elixir: Eine Datumsumwandlung in einen String."
simple_title:         "Eine Datumsumwandlung in einen String."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

"## Was & Warum?"

Das Konvertieren eines Datums in einen String bedeutet, ein Datumsobjekt in ein lesbares Textformat umzuwandeln. Programmierer tun dies, um die Darstellung von Daten zu vereinfachen und zu standardisieren.

"## Wie geht das?"

Elixir bietet die Funktion `Calendar.Format.date_to_string/2`, die ein Datumsobjekt und ein gewünschtes Format als Parameter erhält. Hier ist ein Beispiel, das das aktuelle Datum in dem Format `'YYYY-MM-DD'` ausgibt:

```Elixir
iex> Calendar.Format.date_to_string(~U[2021-10-05], "{YYYY}-{MM}-{DD}")
"2021-10-05"
```

"## Tiefer Einblick"

Das Konvertieren von Daten in Strings ist ein grundlegender Vorgang in der Programmierung, der es ermöglicht, Informationen auf verständliche Weise darzustellen. Früher wurde dies manuell mithilfe von Datumsfunktionen durchgeführt, wodurch die Möglichkeit von Fehlern und Inkonsistenzen bestand. Elixir bietet nun eine integrierte Funktion, die dies effizient und zuverlässig erledigt.

Alternativ können Programmierer auch Bibliotheken wie `Timex` oder `Floki` verwenden, die erweiterte Funktionen für die Datums- und Zeitzonenkonvertierung bieten.

"## Siehe auch"

- [Official Elixir documentation for `Calendar.Format.date_to_string/2`](https://hexdocs.pm/elixir/Calendar.Format.html#date_to_string/2)
- [Timex library](https://hexdocs.pm/timex/Timex.html)
- [Floki library](https://hexdocs.pm/floki/Floki.html)