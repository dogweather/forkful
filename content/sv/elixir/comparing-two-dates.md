---
title:    "Elixir: Jämförelse av två datum"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara en användbar funktion när du arbetar med programering. Det gör det möjligt att sortera, filtrera eller beräkna tidsintervall. I denna bloggpost kommer vi att diskutera hur man jämför två datum i Elixir.

## Hur man gör

För att kunna jämföra två datum behöver vi använda ett inbyggt hjälpmedel i Elixir som kallas `DateTime`. Detta hjälper oss att hantera datum och tider på ett enkelt sätt.

För att börja med måste vi skapa två `DateTime`-objekt som vi vill jämföra. Vi kan göra detta genom att använda funktionen `DateTime.from_naive` som tar in ett datum och en tidszon som argument.

```elixir
DateTime.from_naive({2021, 12, 25}, "UTC")
```
```elixir
DateTime.from_naive({2022, 1, 1}, "UTC")
```

Nu när vi har två datumobjekt, kan vi jämföra dem med hjälp av operatorerna `<`, `>` och `==`. Dessa operatorer jämför inte bara datumen utan även tiden.

```elixir
iex> date1 = DateTime.from_naive({2021, 12, 25}, "UTC")
iex> date2 = DateTime.from_naive({2022, 1, 1}, "UTC")

iex> date1 < date2
true

iex> date1 > date2
false

iex> date1 == date2
false
```

Vi kan också jämföra datumen med hjälp av funktionen `DateTime.compare`. Denna funktion returnerar `-1` om första datumet är mindre än det andra, `0` om de är lika och `1` om det första datumet är större.

```elixir
iex> DateTime.compare(date1, date2)
-1
```

## Fördjupning

När vi jämför två datum i Elixir, tar det även hänsyn till tidszoner. Detta är viktigt att komma ihåg om man arbetar med datum från olika tidszoner.

En annan sak att notera är att om man vill jämföra datum utan att ta hänsyn till tid, kan man använda funktionen `DateTime.without_time`.

```elixir
iex> DateTime.without_time(date1) == DateTime.without_time(date2)
true
```

## Se också

Här är några länkar som kan vara användbara när du arbetar med datum i Elixir:

- [Elixir DateTime dokumentation](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Date dokumentation](https://hexdocs.pm/elixir/Date.html)
- [Elixir Calendar dokumentation](https://hexdocs.pm/elixir/Calendar.html)

Tack för att du läste denna bloggpost om att jämföra datum i Elixir! Hoppas det varit till hjälp för ditt programeringsprojekt. Lycka till!