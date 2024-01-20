---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att parsa ett datum från en sträng innebär att omvandla en textrepresentant av ett datum till ett format som Elixir (eller vilket programmeringsspråk som helst) kan förstå och arbeta med. Programmerare gör detta för att hantera och manipulera datumvärden i sin kod effektivt.

## Hur man gör:

Nedan är ett exempel på hur man kan parsa ett datum från en sträng i Elixir.

```Elixir
iex> start_date = "2019-07-26"
iex> {:ok, date} = Date.from_iso8601(start_date)
iex> date
~D[2019-07-26]
```

I detta exempel ändras strängen "2019-07-26" till ett datumobjekt som Elixir kan hantera.

## Djupdykning

1. Historisk kontext: Många programmeringsspråk har inbyggda funktioner för datumhantering. Elixir använder `Date.from_iso8601` vilket är baserat på ISO 8601 datumstandard.
2. Alternativ: Om du vill parsa datum på andra format än ISO 8601, kan du använda `Timex.parse/2` i Timex-biblioteket.
3. Implementationsdetaljer: `Date.from_iso8601` returnerar ett tuple med `:ok` och datumobjektet om det lyckas. Om parsing misslyckas returneras `:error`.

## Se Även

1. [Elixir Date Official Documentation](https://hexdocs.pm/elixir/Date.html).
3. [ISO 8601 Datumstandard](https://www.iso.org/iso-8601-date-and-time-format.html).