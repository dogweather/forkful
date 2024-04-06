---
date: 2024-01-20 17:30:52.206089-07:00
description: "Slik gj\xF8r du: I Elixir kan du h\xE5ndtere datoer med innebygde moduler\
  \ som `Date`. Tidligere var det mer kronglete, med folk som stolte p\xE5\u2026"
lastmod: '2024-04-05T21:53:41.435628-06:00'
model: gpt-4-1106-preview
summary: "I Elixir kan du h\xE5ndtere datoer med innebygde moduler som `Date`."
title: Beregning av en dato i fremtiden eller fortiden
weight: 26
---

## Slik gjør du:
```elixir
# Legger til 10 dager
ten_days_later = Date.add(~D[2023-04-01], 10)
IO.inspect(ten_days_later)  # => ~D[2023-04-11]

# Trekker fra en måned
one_month_earlier = Date.add(~D[2023-04-01], -30)
IO.inspect(one_month_earlier) # => ~D[2023-03-02]

# Beregner antall dager til neste nyttårsaften
days_until_new_year = Date.diff(~D[2024-01-01], ~D[2023-04-01])
IO.inspect(days_until_new_year)  # => 275
```

## Dykk dypere
I Elixir kan du håndtere datoer med innebygde moduler som `Date`. Tidligere var det mer kronglete, med folk som stolte på tredjepartsbiblioteker eller egne metoder for enkel datohåndtering. Nå tilbyr Elixir enkel manipulasjon av datoer rett ut av boksen.

Alternativer til standard `Date`-modulen inkluderer `Timex`, et populært tredjepartsbibliotek som tilbyr en rekke kraftige funksjoner for tid og datoer.

Det er også verdt å nevne at når man beregner datoer i fortiden eller fremtiden, må du ta hensyn til skuddår og ulike antall dager i månedene – noe Elixir tar høyde for i sine beregninger.

## Se også
- [Elixir's Date documentation](https://hexdocs.pm/elixir/Date.html)
- [Timex documentation](https://hexdocs.pm/timex/Timex.html)
