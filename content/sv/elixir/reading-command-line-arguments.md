---
title:                "Elixir: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa kommandoradsargument är en viktig del av programmering i Elixir eftersom det ger dig möjlighet att interagera med ditt program på ett dynamiskt sätt utan att behöva ändra din kod. Det kan också hjälpa till att skriva mer modulära och anpassningsbara program.

## Hur man gör det

För att läsa kommandoradsargument i Elixir kan du använda funktionen `OptionParser.parse/1` tillsammans med `System.argv/0` som ger dig en lista med alla argument som skickats med i terminalen.

Här är en kodexempel som visar hur man kan använda denna metod för att läsa och skriva ut kommandoradsargument:

```elixir
args = System.argv() # lagrar listan med kommandoradsargument
opts = OptionParser.parse(args) # tar fram argumentet och lagrar dem

IO.puts("Hej " <> opts["name"]) # skriver ut "Hej" tillsammans med namnet som skickats som argument
```

När du kör detta program kommer du att bli ombedd att skicka med ett namn som argument. Till exempel:

```
elixir minapplikation.exs --name Anna
```

Detta kommer att resultera i att programmet skriver ut "Hej Anna" som svar.

## Djupdykning

En viktig aspekt att komma ihåg när man läser kommandoradsargument är att de är strängar och behöver eventuellt konverteras till andra datatyper beroende på vad du vill göra med dem i ditt program. Det är också bra att ha en fallback om ett argument inte skickas med, för att undvika fel i ditt program.

En annan användbar funktion för att hantera kommandoradsargument är `OptionParser.help/1` som skriver ut en hjälp-text som visar hur ditt program kan användas och vilka argument som förväntas.

## Se även

- [Elixir CLI dokumenation](https://hexdocs.pm/elixir/CLI.html)
- [Programmering i Elixir: En introduktion](https://medium.com/@christopherlai/programming-in-elixir-an-introduction-4e1c3c3e8639)
- [Elixir Forum](https://elixirforum.com/)