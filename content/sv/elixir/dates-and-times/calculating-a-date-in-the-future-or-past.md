---
date: 2024-01-20 17:30:40.559247-07:00
description: "Att ber\xE4kna ett datum i framtiden eller f\xF6rflutet inneb\xE4r att\
  \ man adderar eller subtraherar tid fr\xE5n ett specifikt datum. Programmerare g\xF6\
  r detta f\xF6r att\u2026"
lastmod: '2024-03-13T22:44:37.580935-06:00'
model: gpt-4-1106-preview
summary: "Att ber\xE4kna ett datum i framtiden eller f\xF6rflutet inneb\xE4r att man\
  \ adderar eller subtraherar tid fr\xE5n ett specifikt datum."
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutenheten"
weight: 26
---

## Hur gör man:
```elixir
# Lägger till dagar till dagens datum
{:ok, today} = Date.new(2023, 4, 1)
future_date = Date.add(today, 10)
IO.puts(Date.to_string(future_date))
# "2023-04-11"

# Tar bort dagar från dagens datum
past_date = Date.add(today, -5)
IO.puts(Date.to_string(past_date))
# "2023-03-27"
```

## Djupdykning:
I Elixir kan du hantera datum med inbyggda moduler som `Date` och `DateTime`. Förmågan att manipulera datum är viktig sedan början av datorprogrammering – för att hålla koll på händelser över tid. Alternativ till Elixirs inbyggda moduler inkluderar externa bibliotek som `Timex`, som erbjuder ännu fler funktioner för datum- och tidshantering.

När du arbetar med datum är det viktigt att tänka på tidszoner och hur de påverkar beräkningen. Elixirs `DateTime` kan hantera detta, medan `Date` används för datum utan tid och tidszon. I det allra första exemplet adderar vi 10 dagar till dagens datum. Det andra exemplet visar hur vi kan gå tillbaka i tiden genom att subtrahera dagar.

## Se även:
- Elixir officiella dokumentation för `Date` module: [https://hexdocs.pm/elixir/Date.html](https://hexdocs.pm/elixir/Date.html)
- Timex, ett populärt tredjepartsbibliotek för datum- och tidshantering: [https://hex.pm/packages/timex](https://hex.pm/packages/timex)
- Elixir School, en primer om datum och tid i Elixir: [https://elixirschool.com/en/lessons/basics/date_time/](https://elixirschool.com/en/lessons/basics/date_time/)
