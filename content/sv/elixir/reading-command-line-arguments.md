---
title:    "Elixir: Läsning av kommandoradsargument"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
Att läsa kommandoradsargument är en viktig färdighet för alla som arbetar med Elixir-programmering. Genom att kunna läsa in data från kommandoraden kan du göra dina program mer dynamiska och interaktiva. Det kan till och med ge dig möjlighet att anpassa din kod till olika miljöer. I denna bloggpost kommer vi att utforska hur man läser kommandoradsargument i Elixir.

## Hur man gör
För att läsa kommandoradsargument i Elixir använder vi funktionen `System.argv/0`. Den här funktionen returnerar en lista med alla argument som förts in från kommandoraden vid exekveringen av programmet. Låt oss titta på ett exempel:

```Elixir
defmodule ArgumentReader do
  def read() do
    args = System.argv()
    IO.inspect(args)
  end
end
```

För att köra detta program behöver vi gå till terminalen och skriva in:

```
elixir argument_reader.exs arg1 arg2
```

I vårt exempel kommer funktionen `read/0` att skriva ut listan med argument, vilket kommer att se ut så här:

```
[arg1, arg2]
```

Om vi hade skrivit `elixir argument_reader.exs` utan några argument så skulle listan ha varit tom.

## Djupdykning
Förutom att bara läsa in kommandoradsargument kan vi också använda `System.argv/0` för att utföra viss validering eller omvandling av argumenten. Till exempel kan vi konvertera argumenten till heltal och sedan använda dem i vår kod. Låt oss titta på ett annat exempel:

```Elixir
defmodule ArgumentConverter do
  def convert_and_add() do
    args = System.argv()
    int_args = Enum.map(args, &String.to_integer/1)
    sum = Enum.sum(int_args)
    IO.puts("The sum of the arguments is: #{sum}")
  end
end
```

Om vi nu kör `elixir argument_converter.exs 5 10`, kommer vårt program att konvertera argumenten till heltal och sedan räkna ut summan, som i detta fall blir 15. Om ett av argumenten inte kunde konverteras till ett heltal skulle programmet krascha och ge ett felmeddelande.

## Se även
- [System.argv/0 i Elixir Docs](https://hexdocs.pm/elixir/System.html#argv/0)
- [Elixir Command Line Applications - Blogginlägg av Saša Jurić](https://marudp.medium.com/elixir-command-line-applications-6fdcc093692b)
- [Elixir Command Line Tools - Avsnitt ur boken "Programming Elixir" av Dave Thomas](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#command-line-tools)

Tack för att du läste denna bloggpost om att läsa kommandoradsargument i Elixir! Vi hoppas att det har gett dig nyttig kunskap om denna viktiga funktion. Glöm inte att utforska fler resurser för att fördjupa din förståelse och förbättra din Elixir-programmering.