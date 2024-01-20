---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

# Elixir Programmering: Att Skriva Ut Debug Utdata

##
## Vad och Varför?
Att skriva ut debug utdata handlar om att spåra programutvecklingsfel genom att visa variabler eller tillstånd i koden vid körning. Det är ett viktigt steg för programmerare för att snabbt identifiera och lösa koden fel.

## Hur ska man göra det:
Använd `IO.inspect` funktionen för att skriva ut debug information. Den returnerar samma värde som skickades till den, vilket gör det enkelt att infoga överallt i koden:

```Elixir
IO.inspect("Hej Världen!")
```
Det gäller även för mer komplexa datatyper:

```Elixir
IO.inspect(%{ "namn" => "Erik", "ålder" => 27})
```
Utdata för koden ovan blir:
```
"Hej Världen!"
%{"namn" => "Erik", "ålder" => 27}
```
## Djupdykning
`IO.inspect` funktionen gick live med Elixir version 1.0.0, som en bekvämlighetsfunktion för att skriva ut biståndsdata. Alternativet är att använda `IO.puts` och manuellt formatera utdatan, vilket kan vara tidskrävande och invecklat för stora datastrukturer.

`IO.inspect` implementeras med hjälp av `Inspect` protokollet, vilket garanterar korrekt visning av alla typer av data i Elixir. Detta inkluderar allt från enkla datatyper, som strängar och heltal, till mer komplexa som kartor och tuples.

## Se Även
Fler detaljer om `IO.inspect` och `Inspect` protokollet kan hittas i Elixir's officiella dokumentation:

- IO.inspect: https://hexdocs.pm/elixir/IO.html#inspect/2
- Inspect protokollet: https://hexdocs.pm/elixir/Inspect.html