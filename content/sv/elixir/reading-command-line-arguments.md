---
title:                "Elixir: Läsning av kommandoradsargument"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa in kommandoradsargument är en viktig färdighet för alla Elixir-programmerare. Det gör det möjligt för dig att skapa mer dynamiska och anpassningsbara program som kan hantera olika användarinput.

## Så här gör du

Att läsa kommandoradsargument i Elixir är enkelt. För att börja måste du importera modulen `System` genom att skriva `import System` i din fil. Sedan kan du använda funktionen `argv` för att hämta en lista av alla kommandoradsargument som skickas till ditt program. Här är ett enkelt exempel på hur man kan skriva ut alla argument:

```Elixir
import System
IO.puts(argv)
```

Om vi till exempel kör detta program från kommandoraden `elixir args.exs hello world`, kommer vi att få följande utmatning:

```Elixir
["hello", "world"]
```

## Djupdykning

När du väl har hämtat kommandoradsargumenten kan du använda dem på många olika sätt i ditt program. Du kan till exempel använda dem som inmatning för olika funktioner eller skapa dynamiska variabler baserade på argumenten. Det är viktigt att notera att `argv` returnerar en lista av strängar, så du kan behöva konvertera dem till andra datatyper beroende på dina behov.

En annan viktig aspekt att tänka på är hur du hanterar felaktiga eller otillräckliga kommandoradsargument. Det är en god praxis att inkludera felhantering i ditt program så att det inte kraschar om användaren ger felaktig input.

## Se även

- [Elixir - System](https://hexdocs.pm/elixir/System.html)
- [Command Line Arguments in Elixir](https://medium.com/@jwarrendawson/command-line-arguments-in-elixir-b05cdaafd299)
- [Elixir School - Command Line Applications](https://elixirschool.com/sv/lessons/basics/command-line-applications/)