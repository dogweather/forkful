---
title:                "Jämförelse av två datum"
html_title:           "Elixir: Jämförelse av två datum"
simple_title:         "Jämförelse av två datum"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum är en vanlig uppgift inom programmering, särskilt när man arbetar med tidskänsliga applikationer. Det är viktigt att kunna avgöra om ett datum är före, efter eller samtidigt som ett annat datum för att kunna implementera olika logik och funktioner.

## Så här gör du
För att jämföra två datum i Elixir kan du använda funktionen `Date.compare/2`. Den tar emot två datum som argument och returnerar antingen `:before`, `:after` eller `:same` beroende på deras ordning.

```Elixir
iex> Date.compare({2021, 9, 10}, {2021, 9, 5})
:after

iex> Date.compare({2021, 6, 20}, {2021, 6, 20})
:same

iex> Date.compare({2020, 12, 1}, {2021, 1, 1})
:before
```

Som du kan se jämförs året, månaden och dagen i datumen för att avgöra ordningen. Detta innebär att enbart tiden inte övervägs vid jämförelsen.

Om du behöver jämföra två datum inklusive tiden kan du använda funktionen `DateTime.compare/2` istället. Den fungerar på samma sätt som `Date.compare/2` men inkluderar även information om timmar, minuter och sekunder.

```Elixir
iex> DateTime.compare({2021, 9, 15, 12, 30, 0}, {2021, 9, 15, 14, 0, 0})
:before
```

En annan användbar funktion är `Date.diff/2` som beräknar antalet dagar mellan två datum.

```Elixir
iex> Date.diff({2021, 9, 15}, {2021, 9, 10})
5
```

## Djupdykning
Elixir har också möjligheten att jämföra två datum baserat på deras tidszoner med hjälp av funktionen `Calendar.compare/2`.

Det finns också olika funktioner för att jämföra specifika delar av ett datum, till exempel `NaiveDateTime.compare/2` för att jämföra enbart datum och tid utan tidszoninformation och `DateTime.equal/2` för att avgöra om två datum är exakt lika.

## Se även
- [Date module in Elixir](https://hexdocs.pm/elixir/DateTime.html)
- [An introduction to date and time in Elixir](https://www.garysieling.com/blog/introduction-date-time-elixir)
- [Working with dates and times in Elixir](https://pragmaticstudio.com/tutorials/working-with-dates-and-times-in-elixir)