---
title:                "Elixir: Skriva tester"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester kan verka tidskrävande och överflödigt för många programmerare, men det är faktiskt en viktig del av utvecklingsprocessen. Tester hjälper till att hitta och förebygga buggar, vilket sparar tid och möjligtvis frustration längre fram i utvecklingen. Det säkerställer även att koden fungerar som den ska och ger en ökad tilltro till slutprodukten.

## Hur man skriver tester i Elixir

För att skriva tester i Elixir använder man sig av ett inbyggt ramverk vid namn ExUnit. Detta tillhandahåller en mängd olika funktioner och assertioner för att testa olika aspekter av koden. Ett enkelt testfall skulle kunna se ut såhär:

```elixir
defmodule MyModuleTest do
  use ExUnit.Case

  test "addition test" do
    result = MyModule.add(2, 3)
    assert result == 5
  end
end

```

I detta exempel har vi skapat ett testfall med namnet "addition test" som kallar på en funktion i vårt modul "MyModule" och förväntar oss att få ett resultat på 5. Om testet går igenom, så betyder det att vår funktion fungerar som den ska. Om det däremot skulle returnera ett annat resultat, så skulle testet misslyckas och vi skulle behöva gå tillbaka och titta på vår kod för att identifiera problemet.

## Djupdykning i att skriva tester

När man skriver tester är det viktigt att täcka så många fall som möjligt för att säkerställa att koden fungerar som den ska. Det är även bra att inkludera edge-cases, alltså situationer som kan vara ovanliga men ändå måste tas hänsyn till. Det finns även möjlighet att skriva tester för asynkrona funktioner, använda mocks och stubs för att simulera externa anrop, och mycket mer. Det finns en mängd olika resurser tillgängliga för att hjälpa dig att blir en expert på testning i Elixir.

## Se även

- Officiell ExUnit dokumentation (https://hexdocs.pm/ex_unit/)
- Elixir Skolan (https://elixirschool.com/sv/lessons/basics/testing/)
- Test Driven Elixir (https://pragprog.com/book/lmelixir/test-driven-elixir)