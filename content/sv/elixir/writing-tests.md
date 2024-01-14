---
title:    "Elixir: Skriva tester"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva test är en viktig del av utvecklingsprocessen i Elixir programmering. Det hjälper till att säkerställa kodens kvalitet och funktionalitet, samt ger en möjlighet att snabbt hitta eventuella fel eller buggar.

## Hur man skriver tester i Elixir

Att skriva tester i Elixir är enkelt och smidigt, tack vare Elixirs inbyggda testramverk ExUnit. För att skriva ett enkelt test, definiera en funktion som heter "test", lägg sedan till ens  kod som ska testas inom "do"-blocket.

```Elixir
defmodule Math do
    def sum(a, b) do
        a + b
    end
end

defmodule MathTest do
    use ExUnit.Case

    test "should calculate the sum of two numbers" do
        assert Math.sum(2, 3) == 5
    end

    test "should return the correct result for negative numbers" do
        assert Math.sum(-5, 3) == -2
    end
end
```

Kör sedan testerna genom att skriva "mix test" i terminalen. Om alla tester passerar, kommer du se grön text och om något test misslyckas, kommer du se röd text tillsammans med specifik information om felet.

## Djupdykning

För att skriva mer avancerade tester, kan du använda dig av olika funktioner som "asserts" och "refutes" för att kontrollera specifika värden eller beteenden hos din kod. Du kan också använda "setup" och "teardown" funktioner för att förbereda och avsluta tester, samt använda dig av "tags" för att gruppera och köra specifika tester.

Det finns också många olika bibliotek och verktyg tillgängliga för att hjälpa till med testning i Elixir, som till exempel "mocks" för att simulera externa beroenden och "property-based testing" för att testa med slumpmässiga värden.

## Se också

- [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School - Testing](https://elixirschool.com/sv/lessons/basics/testing/)
- [The Power of Elixir Testing](https://engineering.shopify.com/blogs/engineering/deep-dive-into-testing-elixir)