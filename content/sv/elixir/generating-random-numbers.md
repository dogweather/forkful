---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:48:44.412231-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Slumpmässiga tal är data som inte följer något förutsägbart mönster. Inom programmering används de för allt från spellogik till säkerhetsfunktioner.

## Så här gör du:
Generera ett slumpmässigt tal i Elixir:

```elixir
:rand.seed(:exsplus, :os.timestamp())
random_number = :rand.uniform(10)
IO.puts(random_number)
```

Exempelutdata: `7`

Generera en lista med 5 slumpmässiga heltal:

```elixir
random_numbers = Enum.map(1..5, fn _ -> :rand.uniform(100) end)
IO.inspect(random_numbers)
```

Exempelutdata: `[56, 22, 89, 6, 43]`

## Fördjupning
Funktionaliteten för att skapa slumpmässiga tal i Elixir hanteras av Erlang-modulen `:rand`. Tidigare användes `:random`, som är förlegad på grund av mindre optimala algoritmer och distributionsmönster. `:rand.uniform/1` genererar ett heltal mellan 1 och det angivna argumentet, och måste initialiseras med `:rand.seed/1` eller `:rand.seed/2` för att få olika resultat varje gång programmet körs. Seedningen med `:os.timestamp()` är ett bra sätt att säkerställa unika talserie.

## Se även
- [Erlang :rand module documentation](http://erlang.org/doc/man/rand.html)
