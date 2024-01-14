---
title:    "Elixir: Beräkning av ett datum i framtiden eller det förflutna"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna ett datum i framtiden eller förfluten tid är en viktig funktion inom programmering. Det kan användas för att planera möten, skapa påminnelser eller beräkna ålder på ett specifikt datum.

## Hur man gör det
För att beräkna ett datum i Elixir, kan du använda funktionen `Date.add/2`. För att beräkna ett datum i framtiden, ange ett positivt tal som det andra argumentet. För att beräkna ett datum i förfluten tid, använder du ett negativt tal som det andra argumentet. Till exempel:

```Elixir
iex> Date.add(Date.utc_today(), 30)
{:ok, ~N[1970-02-15 00:00:00]}

iex> Date.add(Date.utc_today(), -30)
{:ok, ~N[1969-12-16 00:00:00]}
```

## Djupdykning
För att beräkna datumet för en specifik veckodag i nästa vecka, kan du använda funktionen `Date.next_day/2`. Till exempel, om du vill beräkna nästa fredag:

```Elixir
iex> current_date = Date.utc_today()
~D[2021-09-24]

iex> Date.next_day(current_date, 5)
~D[2021-10-01]
```

För att hämta aktuell tid i en viss tidszon, kan du använda `DateTime.now/1` och specificera tidszonen som ett argument. Till exempel, om du vill hämta aktuell tid i New York:

```Elixir
iex> DateTime.now("America/New_York")
~N[2021-09-24 09:30:00]
```

## Se även
- [Elixir Date modulen](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateTime modulen](https://hexdocs.pm/elixir/DateTime.html)
- [List of Time Zones in Elixir](https://hexdocs.pm/elixir/TZ.html)