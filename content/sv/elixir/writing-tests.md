---
title:                "Skriva tester"
html_title:           "Elixir: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester är när man skriver kod som kontrollerar att ens program fungerar som förväntat. Det är ett sätt för programmerare att säkerställa att deras kod är robust och pålitlig.

## Så här gör du:
```Elixir
defmodule Calculator do
  def add(x, y) do
    x + y
  end
end
```

```Elixir
defmodule CalculatorTest do
  use ExUnit.Case

  test "addition" do
    assert Calculator.add(2, 3) == 5
  end
end
```

Output:
```
.....
Finished in 0.03 seconds
5 tests, 0 failures
```

## Djupdykning:
Att skriva tester är en viktig del av den moderna programutvecklingsprocessen eftersom det hjälper till att förhindra fel och buggar i koden. Det finns också andra alternativ för testning, såsom testdriven utveckling (TDD) som involverar att skriva testerna innan man skriver koden.

Testerna kan också användas för att förbättra kodens utformning och struktur. Genom att skriva tester måste man tänka igenom sin kod och se till att den är lätt att testa och förstå.

I Elixir finns det inbyggda verktyg för testning, vilket gör det enkelt att skapa och köra tester. Det är också möjligt att integrera externa testverktyg för mer avancerade tester.

## Se även:
- Elixir's officiella dokumentation om testning: https://elixir-lang.org/getting-started/introduction.html#testing
- Elixir School's tutorial om testning: https://elixirschool.com/en/lessons/basics/testing/
- Video tutorial om testning i Elixir: https://www.youtube.com/watch?v=fEzBAjBLcY8