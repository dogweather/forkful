---
title:    "Elixir: Läsning av kommandoradsargument"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför
Att lära sig hur man läser in kommandoradsargument är en viktig grundläggande kunskap för alla som vill bli en framgångsrik Elixir-programmerare. Det ger dig möjlighet att interagera med dina program på ett dynamiskt sätt, vilket gör det möjligt att hantera olika användarfall och skapa mer robusta program.

## Hur man gör det
För att läsa in kommandoradsargument i Elixir använder man sig av `System.argv` och konverterar det till en lista. Ett enkelt exempel på hur detta kan göras kan se ut så här:

```Elixir
defmodule ArgReader do
  def read_args() do
    System.argv
  end
end

IO.inspect(ArgReader.read_args())
```

Om vi nu kör detta program med några argument från kommandoraden, som till exempel `elixir arg_reader.ex argument1 argument2`, så kommer output att se ut som följande:

```Elixir
["arg_reader.ex", "argument1", "argument2"]
```

Som du kan se så är kommandot och argumenten nu tillgängliga i form av en lista, som du kan använda för att utföra olika operationer i ditt program.

## Djupdykning
Förutom att helt enkelt läsa in kommandoradsargument kan du också använda Elixirs `OptionParser`-modul för att göra din kod mer robust och hantera felaktiga indata. Detta kan vara särskilt användbart om du förväntar dig en viss typ av indata i ditt program.

En annan intressant funktion är `System.argv0`, som ger dig tillgång till namnet på det aktuella programmet. Detta kan användas för att utföra olika åtgärder beroende på hur programmet startades.

## Se även
- Dokumentation för Elixirs `System`-modul: https://hexdocs.pm/elixir/System.html
- Dokumentation för `OptionParser`-modulen: https://hexdocs.pm/elixir/OptionParser.html
- En guide för att läsa och hantera kommandoradsargument med Elixir: https://www.fatalerrors.org/a/reading-command-line-arguments-with-elixir.html