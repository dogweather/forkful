---
title:                "Elixir: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Elixir är ett otroligt kraftfullt programmeringsspråk som erbjuder många möjligheter för utvecklare. En av dessa är möjligheten att enkelt konvertera datum till strängar. I denna bloggpost kommer vi att utforska varför man skulle vilja göra detta och hur man kan göra det på ett enkelt sätt.

## Så här gör du

Konvertera ett datum till en sträng är enkelt i Elixir. Vi använder funktionen `~D` som står för "Datum" och lägger bara till datumet som en argument. Låt oss titta på ett exempel:

```Elixir
~D[2021-03-24] 
```

Detta kommer att returnera datumet som en sträng "2021-03-24". Vi kan också lägga till en valfri formattering genom att använda `~D{}` och ange den önskade formatmallen.

```Elixir
~D{2021-03-24, {"månad", "dag", "år"}} 
```

Detta ett annat strängformat "mars 24, 2021". Du kan experimentera med olika formatmallar för att få den önskade strängen för ditt program. 

## Djupdykning

Konvertering av datum till sträng är faktiskt mycket mer komplicerat bakom kulisserna. Elixir använder sig av Erlangs bibliotek `calendar` för att göra denna process snabb och effektiv. Det finns också många andra inbyggda funktioner som `~D` som gör det möjligt att hantera datum enkelt utan att behöva oroa dig för komplexa algoritmer.

## Se också

- [Elixir Docs - Date and Time](https://elixir-lang.org/docs/master/DateTime.html)
- [Elixir School - Dates and Times](https://elixirschool.com/en/lessons/basics/dates/)
- [Learn X in Y Minutes - Elixir](https://learnxinyminutes.com/docs/elixir/)