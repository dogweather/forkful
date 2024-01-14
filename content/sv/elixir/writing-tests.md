---
title:                "Elixir: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Varför
Att skriva tester är en viktig del av utvecklingsprocessen och kan hjälpa till att säkerställa att din kod fungerar som den ska. Genom att skriva tester kan du upptäcka buggar och förhindra felaktig funktionalitet, vilket sparar tid och pengar på lång sikt.

## Hur man gör det
För att skriva tester i Elixir kan du använda ramverket ExUnit. Det är enkelt att komma igång med och ger en mängd olika funktioner för att testa din kod.

Att skriva ett test i Elixir är enkelt. Du behöver bara skapa en ny fil med namnet "test.exs" och sedan definiera dina tester i en "test" funktion inuti ett "ExUnit.Case" block. Här är ett exempel på hur du kan testa en enkel funktion som adderar två tal:

```Elixir
defmodule Calculator do
  def add(x, y) do
    x + y
  end
end

defmodule CalculatorTest do
  use ExUnit.Case
  test "adds two numbers" do
    result = Calculator.add(2, 3)
    assert result == 5
  end
end
```

När du kör detta test med kommandot "mix test" kommer du att se att det passerar eftersom resultatet är lika med förväntat resultat. Om någonting ändras i koden och testet inte längre passerar, kommer du att få ett varningsmeddelande och veta att något behöver åtgärdas.

Du kan också använda "assert_raise" funktionen för att testa om en viss exception utlöses av din kod. Detta kan vara användbart för att säkerställa att felaktig input hanteras korrekt.

## Djupdykning
Att skriva tester i Elixir är inte bara en bra vana, det är också en del av språkets filosofi. Eftersom Elixir är ett språk som bygger på funktionell programmering, finns det en hög sannolikhet att det mesta av din kod kommer att bestå av funktioner. Genom att skriva tester för dina funktioner kan du säkerställa att de fungerar korrekt och att ditt program beter sig som det bör.

Elixir uppmuntrar också till att skriva tester för ditt program "från början", vilket betyder att du skriver tester när du skriver din kod istället för att lägga till tester senare. Detta hjälper till att förebygga fel och gör det lättare att hitta och fixa eventuella problem senare.

Ett annat viktigt koncept att förstå när det gäller att skriva tester i Elixir är "mocking". Detta betyder att du kan simulera vissa delar av din kod för att testa olika scenarier utan att faktiskt köra hela programmet.

## Se även
Här är några resurser för dig som vill lära dig mer om att skriva tester i Elixir:

- [ExUnit dokumentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School - Skriv tester](https://elixirschool.com/sv/lessons/advanced/testing/)
- [Elixir funktionell tester med meckning](https://medium.com/@thejakeobrien/functional-testing-in-elixir-with-mocking-371e2f7662f0)