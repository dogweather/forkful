---
title:    "Elixir: Generera slumpmässiga tal"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför
För att utveckla dynamiska och slumpvisa funktioner är det viktigt att kunna generera slumpmässiga tal. Detta är särskilt användbart inom spelutveckling och simuleringar.

## Hur man gör
Det finns flera sätt att generera slumpmässiga tal i Elixir, men det enklaste sättet är att använda funktionen `:rand.uniform/1`. Här är ett exempel på hur man använder denna funktion:

```Elixir
IO.puts(:rand.uniform(100)) # Genererar ett slumpmässigt tal mellan 0 och 100
```

Det är också möjligt att ange ett intervall för slumpmässiga tal genom att ange en Start och Slut parameter för funktionen `:rand.uniform/2`. Här är ett exempel:

```Elixir
IO.puts(:rand.uniform(5,10)) # Genererar ett slumpmässigt tal mellan 5 och 10
```

För att generera en sekvens av slumpmässiga tal kan man använda sig av funktionen `Enum.map/2`. Här är ett exempel på hur man kan använda denna funktion för att generera en lista med 10 slumpmässiga tal mellan 0 och 100:

```Elixir
Enum.map(1..10, fn(_) -> :rand.uniform(100) end) # Genererar en lista med 10 slumpmässiga tal mellan 0 och 100
```

## En djupdykning
Elixir använder sig av en teknik som kallas "Mersenne Twister" för att generera slumpmässiga tal. Detta är en mycket effektiv algoritm som är utvecklad för att minimera förutsägbarhet och ge en jämn fördelning av slumpmässiga tal.

Det finns också andra funktioner för att generera slumpmässiga tal i Elixir, såsom `:rand.uniform/0` som genererar en flytande punkt mellan 0 och 1, och `:rand.normal/2` som genererar ett tal från en normalfördelning med angiven medelvärde och standardavvikelse.

## Se även
- [Elixir Dokumentation om Slumpmässiga Tal](https://hexdocs.pm/elixir/Random.html)
- [Elixir Forum Diskussion om Slumpmässiga Tal](https://elixirforum.com/t/randomness-in-elixir/24524)