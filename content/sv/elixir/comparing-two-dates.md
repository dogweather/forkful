---
title:                "Elixir: Jämföra två datum"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum i Elixir kan vara en användbar funktion för att hantera och organisera data som relaterar till tid. Det kan hjälpa till att identifiera mönster, skapa tidslinjer och filtrera data baserat på datum.

## Så här gör du

För att jämföra två datum i Elixir kan du använda funktionen `Date.compare/2`. Detta kommer att ta två datum som argument och returnera en av tre möjliga svarsalternativ: `:before`, `:equal` eller `:after`. Låt oss titta på ett enkelt exempel:

```elixir
# Skapar två datumobjekt
date1 = Date.new(2020, 8, 1)
date2 = Date.new(2020, 8, 15)

# Jämför de två datumobjekten
Date.compare(date1, date2) #=> :before
```

I detta exempel är `date1` tidigare än `date2` vilket ger oss `:before` som svar. Här är några andra möjliga kombinationer av resultat:

```elixir
Date.compare(date2, date1) #=> :after
Date.compare(date1, date1) #=> :equal
```

Funktionen `Date.compare/2` tar också hänsyn till skottår och månadslängd, vilket gör den tillförlitlig för alla typer av datum.

## Djupdykning

I bakgrunden använder `Date.compare/2` funktionen `Calendar.Date.compare/2`, vilket ger oss en mer detaljerad insikt i hur jämförelsen faktiskt sker. `Calendar.Date.compare/2` använder sig av en algoritm som jämför året, månaden och dagen i respektive datum och returnerar ett av fem möjliga svar: `:before`, `:equal`, `:after`, `:ends_before` eller `:ends_after`.

`ends_before` och `ends_after` är resultat som bara kan erhållas när skillnaden mellan årets skottår och resterande dagar i månaden är 7 dagar eller mindre. Dessa resultat indikerar att ett datum är före eller efter ett annat datum på grund av skottår.

## Se även

- [Date.compare/2 Documentation](https://hexdocs.pm/elixir/Date.html#compare/2)
- [Calendar.Date.compare/2 Documentation](https://hexdocs.pm/elixir/Calendar.Date.html#compare/2)
- [Elixir Date and Time Functions](https://dev.to/owensullivan/elixir-date-and-time-functions-1ped)
- [Elixir Date Module Overview](https://hackernoon.com/a-clean-elixir-date-module-rph-43c8f9cb542)