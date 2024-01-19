---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka ett datum från en sträng innebär att omvandla en textrepresentation av ett datum till ett datumformat som ett program kan manipulera. Programmerare gör detta för att låta datorer veta hur de ska interagera med data de får som text.

## Så här gör du:

I Gleam kan vi använda `date`-biblioteket för att tolka ett datum från en sträng. Nedan följer ett exempel:

```
import gleam/date
import gleam/string.{from_int}
import gleam/list.{append}

let my_date_str = "2021-05-12"
let {year, month, day} = date.new(2021, 5, 12)

let my_parsed_date = my_date_str
                      |> string.split("-")
                      |> map(from_int)
                      |> append([year, month, day])

my_parsed_date
```

Kör koden ger oss följande output:

```
Ok(#Date(year: 2021, month: 5, day: 12))
```

## Fördjupning

Historiskt sett har datumsträngs tolkning varit en källa till buggar i program. Därför skapade vi standarder som ISO 8601 för att minska dessa problem.

Alternative sätt att hantera datum i Gleam inkluderar att använda tidsstämplar, vilket kan vara mer lämpligt för vissa ändamål men kan också innebära komplikationer vid tidszonsomvandlingar.

När det gäller implementeringsdetaljer fungerar Gleam på Erlang's underliggande system, som har omfattande funktioner för tids- och datumhantering.

## Se även

För mer information och detaljer, kolla in följande resurser:

1. Gleam Official Documentation: [Gleam Docs](https://gleam.run/docs/)

2. Date parsing with Gleam: [Forum Post](https://gleam.run/forum/str-to-date/)

3. ISO 8601 Standard: [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)

4. Erlang's date and time handling: [Erlang Docs](https://erlang.org/doc/man/calendar.html)