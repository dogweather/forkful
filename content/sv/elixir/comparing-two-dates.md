---
title:                "Elixir: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en viktig färdighet inom programmering och kan hjälpa till att avgöra rätt ordning för händelser eller beräkna åldern på något. I denna bloggpost kommer vi att gå igenom hur man kan jämföra två datum i Elixir och utforska djupare i konceptet.

## Hur man gör det

För att jämföra två datum används funktionen `Date.compare/2` i Elixir. Det första argumentet är det första datumet som ska jämföras och det andra argumentet är det andra datumet. I exemplet nedan jämför vi två datum och skriver ut resultatet:

```Elixir
date1 = Date.new(2021, 10, 15)
date2 = Date.new(2021, 9, 15)

IO.puts "Resultatet av jämförelsen är #{Date.compare(date1, date2)}"
```

Output:

```
Resultatet av jämförelsen är :gt
```

Funktionen `Date.compare/2` returnerar en atom som representerar resultatet av jämförelsen. Här är en lista över de olika värden som kan returneras:

- `:lt` - Det första datumet är mindre än det andra
- `:eq` - Båda datumen är lika
- `:gt` - Det första datumet är större än det andra

## Djupdykning

För att förstå hur `Date.compare/2` fungerar, behöver vi först förstå hur datum representeras i Elixir. I Elixir finns det två typer av datum: `Date` och `DateTime`. `DateTime` innehåller både datum och tid medan `Date` bara innehåller datumet.

När vi jämför två datum i Elixir jämförs de som tuples med tre värden: år, månad och dag. Det är därför `Date.compare/2` returnerar antingen `:lt`, `:eq` eller `:gt` beroende på vilket datum som är större i varje värde.

Det är också värt att nämna att `Date`-modulen innehåller andra användbara funktioner för att jämföra datum, som `Date.before?/2` och `Date.after?/2` som returnerar en boolean baserat på jämförelsen.

## Se även

- [Date-modulen i Elixir dokumentationen](https://hexdocs.pm/elixir/Date.html)
- [Elixir Programming Language - Officiell hemsida](https://elixir-lang.org/)