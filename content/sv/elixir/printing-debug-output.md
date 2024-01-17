---
title:                "Utmatning av felsökningsinformation"
html_title:           "Elixir: Utmatning av felsökningsinformation"
simple_title:         "Utmatning av felsökningsinformation"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva ut debuggning är en vanlig metod som programmerare använder för att hitta fel i sin kod. Genom att skriva ut information om vad som händer i programmet vid olika steg, kan man lättare identifiera och åtgärda eventuella buggar.

## Hur man gör:

För att skriva ut debuggning i Elixir, kan du använda funktionen IO.inspect(). Detta kommer att skriva ut värdet på en variabel eller ett uttryck. Här är ett exempel på hur man gör det:

```Elixir
iex> name = "Maria"
iex> IO.inspect(name)
"Maria"
```

Det finns också möjlighet att skriva ut flera variabler samtidigt genom att skriva dem i en lista:

```Elixir
iex> age = 25
iex> city = "Stockholm"
iex> IO.inspect([age, city])
[25, "Stockholm"]
```

## Djupdykning:

Att skriva ut debuggning är en viktig del av felsökning i programmering. Det hjälper till att förstå vad som pågår i koden vid olika steg och kan användas för att hitta och åtgärda fel snabbare.

Förutom att skriva ut värden på variabler och uttryck, kan du också använda dig av funktionen Logger i Elixir. Detta ger möjlighet att skriva ut meddelanden med olika nivåer av allvarlighet, vilket kan vara användbart för att filtrera och hantera utskrifter på ett mer specifikt sätt.

Du kan också använda alternativ som Pry, som ger mer avancerade funktioner för att skriva ut debuggning och interagera med programmet i en körunderbrytare. Detta kan vara användbart för mer komplexa situationer där en enkel utskrift inte räcker.

## Se även:

- [Elixir dokumentation om IO.inspect()](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Elixir dokumentation om Logger](https://hexdocs.pm/logger/Logger.html)
- [Pry Elixir](https://github.com/pry/pry)