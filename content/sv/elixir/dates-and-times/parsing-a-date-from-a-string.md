---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 02:05:09.671092-07:00
description: "Hur man g\xF6r: I Elixir kan du tolka datum genom att anv\xE4nda modulen\
  \ `Date`. S\xE5 h\xE4r omvandlar du en str\xE4ng till ett datum."
lastmod: '2024-03-13T22:44:37.576834-06:00'
model: gpt-4-0125-preview
summary: "I Elixir kan du tolka datum genom att anv\xE4nda modulen `Date`."
title: "Att tolka ett datum fr\xE5n en str\xE4ng"
weight: 30
---

## Hur man gör:
I Elixir kan du tolka datum genom att använda modulen `Date`. Så här omvandlar du en sträng till ett datum:

```elixir
date_string = "2023-04-05"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)
```

Exempel på utdata:

```elixir
~D[2023-04-05]
```

För att hantera olika format kan du använda biblioteket `Timex`:

```elixir
{:ok, datetime} = Timex.parse("05-04-2023", "{D}-{0M}-{YYYY}")
IO.inspect(datetime)
```

Exempel på utdata:

```elixir
#DateTime<2023-04-05 00:00:00Z>
```

## Fördjupning
Funktionen `Date.from_iso8601/1` är en del av Elixirs standardbibliotek, introducerad för att säkerställa enkel tolkning av ISO8601-standarden för datum - ett vanligt datumformat. Men livet är inte så enkelt; datum kommer i massor av format. Det är här `Timex`, ett tredjeparts Elixir-bibliotek, kommer in i bilden. Det är rikare än de inbyggda datumfunktionerna i Elixir och hjälper till att hantera en mängd olika datumformat.

Elixir självt är oföränderligt, vilket innebär att tolkade datum är inget undantag; de kan inte ändras när de väl skapats. Denna funktion går tillbaka till de funktionella programmeringsrötterna hos Elixir, vilket garanterar förutsägbarhet och enklare felsökning.

Historiskt sett har datumtolkning varit svårt på grund av varierande standarder. Men med bibliotek som `Timex` och språkfunktioner i Elixir, abstraheras komplexiteten bort, vilket gör en utvecklares liv lite enklare.

## Se också
- [Elixir Datum](https://hexdocs.pm/elixir/Date.html)
- [Timex Dokumentation](https://hexdocs.pm/timex/Timex.html)
- [ISO8601 Standard](https://www.iso.org/iso-8601-date-and-time-format.html)
