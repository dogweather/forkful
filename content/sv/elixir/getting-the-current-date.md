---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför
Att hämta dagens datum innebär att programmerare får exakt tid och datum info. Detta är nödvändigt för att logga händelser, tidstämpa data eller anpassa användarupplevelser.

## Hur man gör
Här är ett sätt att få aktuell tid och datum i Elixir:

```elixir
iex> DateTime.utc_now()
~U[2022-01-01T12:00:00Z]
```

Notera att `DateTime.utc_now()` återger aktuell tid i UTC-format.

## Djupdykning

Att få aktuellt datum är ett gammalt koncept som går tillbaka till tidiga dagar av programmering. I Elixir används `DateTime` modulen, som lanserades i version 1.3, för att hantera datum och tid. 

Alternativ till `DateTime` inkluderar `Date` och `NaiveDateTime` modulerna. `Date` ger endast datuminformation, medan `NaiveDateTime` ger datum och tid utan tidszoninformation.

Detaljer om implementation: När `DateTime.utc_now()` kallas, hämtas det aktuella datumet och tiden från datorns systemklocka och konverteras till ett `DateTime`-struktur i UTC.

## Se också

1. [`DateTime` dokumentation](https://hexdocs.pm/elixir/DateTime.html) - Fullständig dokumentation för DateTime modulen.
2. [`Date` dokumentation](https://hexdocs.pm/elixir/Date.html) - Användbart om du bara vill hantera datum.
3. [`NaiveDateTime` dokumentation](https://hexdocs.pm/elixir/NaiveDateTime.html) - Utforska detta om du inte behöver tidszonsinfo.
4. [Tidszonsdatabas i Elixir](https://hexdocs.pm/elixir/TimeZoneDatabase.html) - Lär dig om hur Elixir hanterar tidszoner.