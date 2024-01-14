---
title:                "Elixir: Skapa slumpmässiga nummer"
simple_title:         "Skapa slumpmässiga nummer"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Varför

Att generera slumpmässiga tal är en viktig del av många programmeringsprojekt. Det kan användas för att skapa spel, utföra tester eller skapa unika ID-nummer. Elixir erbjuder ett enkelt och kraftfullt sätt att generera slumpmässiga tal som passar både nybörjare och erfarna programmerare.

# Så här gör du

För att börja generera slumpmässiga tal i Elixir, behöver du först importera modulen `:rand`. Sedan kan du använda funktionen `uniform/0` för att generera ett tal mellan 0 och 1. Om du vill ha ett tal inom ett visst intervall, som till exempel mellan 1 och 10, kan du använda funktionen `uniform/1` och ange en lista med de två talen som parametrar.

```elixir
import :rand

rand.uniform() # Genererar ett slumpmässigt tal mellan 0 och 1
rand.uniform([1, 10]) # Genererar ett slumpmässigt tal mellan 1 och 10
```

Du kan också använda funktionen `rand_between/2` för att generera ett heltal mellan två tal. Om du vill generera ett slumpmässigt tal från en lista av möjliga värden, kan du använda funktionen `rand_seed/1` för att ange en "frö"-parameter och sedan använda `rand/1` för att välja ett slumpmässigt värde från listan baserat på detta frö.

```elixir
rand_between(1, 10) # Genererar ett slumpmässigt heltal mellan 1 och 10
rand_seed(123) # Ange frö för att generera samma värde varje gång
rand(["Elixir", "Programmering", "Slumpmässiga tal"]) # Genererar ett slumpmässigt element från listan
```

# Djupdykning

Slumpmässiga tal i Elixir genereras genom en pseudorandom generator. Detta innebär att värdena som genereras inte är helt slumpmässiga utan bygger på en algoritm som tar en "frö"-parameter och genererar ett nummer baserat på detta. Om fröet är detsamma kommer även det genererade talet att vara detsamma. Detta kan vara användbart för testning men kan också leda till förutsägbara resultat om fröet inte ändras.

En annan aspekt av det slumpmässiga nummergenereringsprocessen är att den inte är helt beroende av processorn eller systemklockan. Detta betyder att även om klockan uppdateras, kommer de genererade värdena att vara desamma - en viktig funktion för stabila tester.

# Se även

- Elixir: https://elixir-lang.org/
- Hitta slumpmässiga tal: https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%3C%3C%3E%3C%3E/1
- Slumpmässiga tal i andra programmeringsspråk: https://www.geeksforgeeks.org/generating-random-number-list-in-python/