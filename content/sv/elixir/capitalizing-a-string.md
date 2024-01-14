---
title:                "Elixir: Att skriva ut en textsträng i versaler"
simple_title:         "Att skriva ut en textsträng i versaler"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera en sträng till kapitaliserad form, det vill säga där alla ord börjar med versaler, är en användbar funktion inom Elixir-programmering. Det gör det enklare att hantera data och presentera den på ett snyggt sätt för användaren.

## Hur man gör det

För att konvertera en sträng till kapitaliserad form i Elixir, kan man använda sig av funktionen `String.capitalize/1`. Detta gör att det första tecknet i strängen blir en versal och alla andra tecken blir gemena. Till exempel:

```Elixir
iex> String.capitalize("hej världen")
"Hej världen"
```

Om man möter på en sträng som redan är kapitaliserad eller där varje ord börjar med en versal, kommer funktionen inte att ändra något. Detta kan vara till nytta när man vill hantera olika typer av data.

För att kapitalisera varje ord i en sträng, kan man använda funktionen `String.capitalize/2` och ange värdet `:first` för det andra argumentet. Detta kommer att göra att alla ord i strängen börjar med en versal. Till exempel:

```Elixir
iex> String.capitalize("hej världen", :first)
"Hej Världen"
```

## Djupdykning

Elixir är baserat på funktionell programmering, vilket innebär att funktioner är första klassens medborgare. Detta innebär att de kan användas som variabler, skickas som argument till andra funktioner och returneras. Detta är vad som händer när vi anropar `String.capitalize/1` och `String.capitalize/2`. Funktionen `capitalize` är bara en av många inbyggda funktioner som gör det möjligt för oss att hantera strängar på olika sätt.

En intressant aspekt hos `String.capitalize/2` är att den kan anpassas genom att ange en språkkod som tredje argument. Detta gör att funktionen kan hantera språkspecifika regler för kapitalisering. Till exempel:

```Elixir
iex> String.capitalize("django reinhardt", :first, :fr)
"Django Reinhardt"
```

## Se också

Här är några användbara länkar till mer information om Elixir-programmering och hantering av strängar:

- [Elixir Programming Language](https://elixir-lang.org/)
- [String.capitalize/1](https://hexdocs.pm/elixir/String.html#module-capitalize)
- [String.capitalize/2](https://hexdocs.pm/elixir/String.html#module-capitalize-2)
- [Funktionell programmering](https://en.wikipedia.org/wiki/Functional_programming)