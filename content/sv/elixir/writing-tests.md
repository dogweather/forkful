---
title:    "Elixir: Skriva tester"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

När man programmerar är det viktigt att inte bara skriva kod, utan också att se till att koden fungerar som den ska. Det är där testning kommer in i bilden. Genom att skriva tester kan du vara säker på att din kod fungerar som den ska och dessutom undvika potentiella buggar och felaktigheter.

## Hur man gör det

En av de mest populära testramverken för Elixir är ExUnit, som erbjuder många funktioner för att skriva välstrukturerade och pålitliga tester. Låt oss ta en titt på hur man kan skriva ett enkelt test med ExUnit.

```Elixir
defmodule Test do
  use ExUnit.Case
  
  test "summera två tal" do
    result = Calculator.add(5, 7)
    assert result == 12
  end
end
```

Här skapar vi en test-modul och använder sig av ExUnit.Case. Vi definierar sedan en testfunktion genom att använda nyckelordet "test". Inuti testet kör vi vår kod som vi vill testa, och sedan använder vi funktionen "assert" för att kontrollera om resultatet är det förväntade värdet.

När vi kör testet genom att köra "mix test" i terminalen får vi följande output:

```Elixir
1 test, 0 failures
```

Om testet skulle misslyckas, skulle vi se en felmeddelande och programmet skulle sluta köra. Detta ger oss ett enkelt sätt att se till att vår kod fungerar som den ska.

## Djupdykning

När det gäller testning finns det många olika tekniker och strategier som kan användas för att skriva effektiva tester. Här är några tips för att bli en bättre tester i Elixir:

- Följ "Arrange, Act, Assert" mönstret för att hålla dina tester välstrukturerade och lättlästa.
- Använd "case" uttryck för att testa flera olika scenarier inom samma test.
- Använd "setup" och "teardown" funktioner för att sätta upp och ta bort testresurser.
- Testa både positiva och negativa fall för att se till att din kod hanterar alla möjliga scenarier.

Se även

- [ExUnit dokumentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir Test Patterns](https://github.com/anantas/the-elixir-test-patterns)
- [Elixir School - Testing](https://elixirschool.com/en/lessons/basics/testing/)

Testning är en viktig del av att skriva pålitlig kod och med ExUnit och dessa tips kan du börja skriva effektiva tester för din Elixir-kod. Lycka till!