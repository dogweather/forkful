---
title:                "Utskrift av felsöksutdata"
html_title:           "Elixir: Utskrift av felsöksutdata"
simple_title:         "Utskrift av felsöksutdata"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod är som att lösa ett pussel, och ibland kan saker gå fel. Det kan vara svårt att förstå varför koden inte fungerar som den ska, och därför är det viktigt att kunna skriva ut debugginformation för att få en bättre förståelse för vad som händer i programmet.

## How To

För att skriva ut debugginformation i Elixir använder du funktionen `IO.inspect`. Den tar in vilken som helst typ av data och skriver ut den till konsolen. Här är ett enkelt exempel:

```Elixir
name = "John"
IO.inspect(name) 
```

Detta kommer att skriva ut värdet av variabeln `name`, i detta fall "John", till konsolen.

Du kan också skriva ut flera värden samtidigt genom att lägga till en kommatecken mellan dem:

```Elixir 
IO.inspect(name, age, hobby)
```

Detta kommer att skriva ut värdena av `name`, `age` och `hobby` till konsolen.

Om du behöver skriva ut debugginformation i en funktion kan du använda nyckelordet `IO.inspect` innan en variabel för att skriva ut dess värde:

```Elixir 
def add_numbers(a, b) do 
  result = a + b 
  IO.inspect(result) 
  result 
end 
```

I detta exempel kommer värdet av `result` att skrivas ut innan det returneras från funktionen.

## Deep Dive

Det finns några fler funktioner och alternativ som kan hjälpa dig att skriva ut debugginformation på ett effektivt sätt:

- Du kan använda `IO.inspect` i en pipeline (sekvens av funktioner som körs en efter en) genom att använda atomerna `:label` och `:inspect` istället för att skriva om hela `IO.inspect`-funktionen. Exempel: `|> :label |> :inspect`.
- Du kan använda `IO.inspect` på en rad med `|>` för att visa värdet som skickas mellan funktionerna i pipelinen.
- För att avaktivera `IO.inspect` i produktion kan du använda `Logger.debug` istället.
- Om du vill få reda på vilken rad kod som printade ut en viss debugg-output kan du använda funktionen `__ENV__`.ex_line för att få den raden.

Med dessa verktyg kan du enkelt skriva ut debugginformation när du behöver det och få en bättre förståelse för vad som händer i din kod.

## Se också

- Officiell Elixir dokumentation för IO.inspect funktionen: https://hexdocs.pm/elixir/IO.html#inspect/2
- En guide för debuggning i Elixir på Elixir School: https://elixirschool.com/sv/lessons/advanced/debugging/