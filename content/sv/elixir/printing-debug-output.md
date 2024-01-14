---
title:                "Elixir: Utskrift av felsökningsdata"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debug-output är ett viktigt verktyg för utvecklare som vill förstå vad som händer i sina program. Det kan hjälpa dig att hitta och lösa felaktig kod, och det kan också ge dig förståelse för hur dina program fungerar internt.

## Hur man gör

Det enklaste sättet att skriva ut debug-output i Elixir är att använda funktionen `IO.inspect/2`. Detta tar ett värde som argument och skriver ut det till terminalen.

```Elixir
IO.inspect("Hej världen")
```

Detta kommer att skriva ut "Hej världen" till terminalen. Du kan också använda `IO.inspect/2` för att skriva ut variabler eller värden i dina program.

```Elixir
namn = "Elixir"
IO.inspect(namn)
```

Detta kommer att skriva ut värdet av variabeln "namn" till terminalen.

Du kan också formatera utskriften med hjälp av formatsträngar, precis som i andra programmeringsspråk. Till exempel:

```Elixir
IO.inspect("Välkommen %s", "till Elixir") 
```

Denna kod kommer att skriva ut "Välkommen till Elixir" till terminalen.

## Utforska djupare

För mer avancerade debugging-scenarier kan du använda Elixirs inbyggda `:debugger` modul. Detta ger dig tillgång till en interaktiv debugger där du kan stega igenom din kod och undersöka variabler och uttryck.

Du kan också använda `Logger` modulen för att skriva ut debug-information till loggfiler. Detta är särskilt användbart för långsiktiga debugginsättningar eller för att spåra problem i produktion.

## Se också

- [Elixir Dokumentation om IO.inspect/2](https://hexdocs.pm/elixir/Kernel.html#inspect/2)
- [Elixir Dokumentation om :debugger](https://hexdocs.pm/elixir/Debugger.html)
- [Elixir Dokumentation om Logger-modulen](https://hexdocs.pm/logger/Logger.html)