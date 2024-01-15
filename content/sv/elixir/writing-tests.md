---
title:                "Att skriva tester"
html_title:           "Elixir: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Varför 

Att skriva tester är ett viktigt steg i utvecklingsprocessen eftersom det hjälper till att säkerställa att koden fungerar som det är tänkt och minskar risken för buggar i produktion. Det hjälper också till att spara tid och pengar på lång sikt genom att undvika fel och buggfixning i efterhand. 

## Så här 

För att skriva tester i Elixir använder man huvudsakligen ett ramverk som heter ExUnit. Det ingår i Elixir och behöver inte installeras separat. Vi kommer att titta på ett enkelt exempel på hur man kan skriva tester i Elixir. 

```Elixir
defmodule CalculatorTest do 
  use ExUnit.Case 
  
  test "addition" do 
    assert Calculator.add(2, 3) == 5 
  end 
end
```

Vi börjar med att skapa ett modul som heter CalculatorTest och använder oss av ExUnit.Case. Sedan skapar vi en testfunktion där vi använder assert för att jämföra värdet av funktionen add() från modulen Calculator med det förväntade resultatet. Om testet passerar kommer vi att se något som detta i terminalen:

```
1 test, 1 assertion, 0 failures.
````

## Djupdykning

Det finns många olika sätt att skriva tester i Elixir och ExUnit erbjuder en mängd olika funktioner och metoder för att skapa testfall. Ett av de mest använda är "setup" som låter dig göra inställningar innan ett test körs. Detta är särskilt användbart när du behöver göra vissa förberedelser innan du testar en funktion. 

Det finns också funktionen "setup_all" som låter dig göra inställningar för alla tester i en modul. Detta kan vara användbart om du behöver ansluta till en databas eller hämta data som behövs för flera tester. 

För att läsa mer om hur man skriver tester i Elixir kan du kolla in ExUnit-dokumentationen [här](https://hexdocs.pm/ex_unit/ExUnit.html) och [här](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#testing-your-project).

## Se även 

- [Elixir](https://elixir-lang.org/)
- [ExUnit-dokumentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Mix-dokumentation](https://hexdocs.pm/mix/Mix.html)
- [Elixir School](https://elixirschool.com/sv/lessons/basics/testing/)