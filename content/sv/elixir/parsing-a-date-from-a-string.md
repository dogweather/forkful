---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:35:29.304858-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att omvandla en datumsträng handlar om att tolka texten som ett datumobjekt; detta gör vi för att kunna hantera datum matematiskt och logiskt i våra program.

## Så Här Gör Du:

```elixir
# Använder Elixir's inbyggda DateTime-modul
date_string = "2023-04-12T16:30:00Z"
{date, _} = DateTime.from_iso8601(date_string)
IO.inspect(date)
```

Utskrift:
```elixir
#DateTime<2023-04-12 16:30:00Z>
```

## Fördjupning:

I Elixir använder vi `DateTime`-modulen för att hantera datum och tid. Historiskt sett har hantering av datum och tid varit en komplex uppgift på grund av tidszoner och olika format. Elixir hanterar detta elegant med inbyggda funktioner. Ett alternativ är också att använda paket som `Timex` för mer komplex funktionalitet. `DateTime.from_iso8601` är en rak metod för att omvandla ISO 8601-formatsträngar till datumobjekt, men du måste hålla koll på felhantering om strängen inte följer rätt format.

## Se Också:

- Elixir's dokumentation för `DateTime`: https://hexdocs.pm/elixir/DateTime.html
- `Timex`, ett tredjepartsbibliotek för datum och tid i Elixir: https://hexdocs.pm/timex/readme.html
- ISO 8601 datum- och tidsstandarden: https://www.iso.org/iso-8601-date-and-time-format.html