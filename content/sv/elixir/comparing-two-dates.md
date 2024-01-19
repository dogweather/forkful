---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Jämförelse av två datum är en teknik som används för att bestämma skillnaden mellan tidsintervaller. Det är viktigt för programmerare att utföra dessa jämförelser för viktiga funktioner som händelseplanering, tidsspårning och mer.

## Hur gör man:
I Elixir kan du jämföra två datum med hjälp av `Date.compare/2` funktion.

```Elixir
date1 = ~D[2022-02-10]
date2 = ~D[2022-02-20]

str = Date.compare(date1, date2)

IO.puts("Date Comparison: #{str}")
```

**Sample output:**

```
Date Comparison: :lt
```

Ovanstående kod jämför `date1` och `date2`. När `date1` är tidigare än `date2` returneras `:lt`, vilket betyder att `date1` är mindre än `date2`     

## Djupdykning
Historiskt sett har datumjämförelse varit en del av programmering sedan dess början. I Elixir introducerades `Date.compare/2` funktionen i version 1.3 som en del av den stadigt växande standardbiblioteket.

En alternativ metod för att jämföra datum är att omvandla dem till sekunder sedan Epoken (1970-01-01 00:00:00 UTC) och sedan jämföra dessa värden, men `Date.compare/2` är både enklare och mer läsbar.

Inget förlorat i fråga om prestanda heller. Elixir's `Date.compare/2` är skriven i Erlang som gör datumjämförelser snabba och minneseffektiva.

## Se även
För mer information om datumjämförelser i Elixir, checka:
- [Elixir School](https://elixirschool.com/en/lessons/basics/date_time) för mer om datum och tid i Elixir.
- [Elixir Date docs](https://hexdocs.pm/elixir/Date.html#compare/2) för detaljerad dokumentation om `Date.compare/2`.