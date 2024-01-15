---
title:                "Generering av slumpmässiga nummer"
html_title:           "Elixir: Generering av slumpmässiga nummer"
simple_title:         "Generering av slumpmässiga nummer"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en viktig del av många program, inklusive spel, simuleringar och kryptografi. Med hjälp av slumpmässiga nummer kan man skapa variation och osäkerhet, vilket gör det möjligt att skapa mer realistiska och säkra program.

## Så här gör du

```Elixir
# Generera ett slumpmässigt heltal mellan 1 och 10
IO.puts("Här är ett slumpmässigt tal mellan 1 och 10:")
IO.puts(Enum.random(1..10))

# Generera en lista med 5 slumpmässiga frukter
IO.puts("Här är en slumpmässig lista med frukter:")
IO.inspect(Enum.shuffle(["äpple", "banan", "apelsin", "jordgubbe", "kiwi"]))
```

Output:

Här är ett slumpmässigt tal mellan 1 och 10:
7
Här är en slumpmässig lista med frukter:
["äpple", "jordgubbe", "banan", "apelsin", "kiwi"]

För att generera slumpmässiga tal i Elixir använder man funktionen `Enum.random/1` och anger ett intervall för de möjliga numren. Man kan välja att generera enstaka tal eller en lista med flera slumpmässiga värden genom att använda funktionen `Enum.shuffle/1` och ange en lista med de önskade värdena.

## Djupdykning

Elixir använder sig av en intern generator, kallad "UniformDistribution", som genererar slumpmässiga nummer baserat på en seed. En seed är ett startvärde som används för att generera en följd av siffror som ser slumpmässiga ut. Om man vill ha mer kontroll över genereringen av slumpmässiga tal kan man ange sin egen seed genom att använda funktionen `:uniform.seed/1`.

En viktig sak att tänka på är att "UniformDistribution" inte är en kryptografiskt säker generator, vilket betyder att den inte bör användas i situationer där säkerhet är avgörande.

## Se även

- [Elixir Enum-modulen](https://hexdocs.pm/elixir/Enum.html)
- [Slumpmässiga nummer i Elixir - Elixir Cheat Sheet](https://elixirschool.com/sv/cheatsheets/random/)
- [Elixir Random-modulen](https://hexdocs.pm/elixir/Random.html)