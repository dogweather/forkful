---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Elixir och Kommandoradsargument: En enkel guide

## Vad och varför?

Kommandoradsargument är data som du ger till ditt program när du kör det. Programmerare använder detta för att anpassa programmets beteende utan att behöva ändra koden.

## Så här gör du:

Elixir använder `System.argv/0` för att läsa argumenten. Det returnerar en lista med strängar. Varje argument är ett element.

``` Elixir
IO.inspect System.argv()
``` 

Om du kör programmet med `elixir myfile.exs arg1 arg2`, kommer det att skriva ut `["arg1", "arg2"]`.

## Djupdykning

`System.argv/0` är en del av Elixir sedan dess tidiga versioner, liksom i många andra språk. 

Alternativa sätt att ta emot data inkluderar filinmatning, trådade meddelanden eller nätverkskommunikation. Men användning av kommandoradsargument är ofta det mest praktiska för snabba uppgifter och engångsskript.

Detaljer: `System.argv/0` är ett samtal till Erlangs `:init.get_plain_arguments/0` - Elixir bygger ju på Erlang VM. 

## Se även

För mer om `System.argv/0`, besök [Elixir officiella dokumentation](https://hexdocs.pm/elixir/System.html#argv/0). 

Mer om hur Elixir hanterar kommandoradsargument kan du läsa på [Elixir skoll's tutorial](http://elixirschool.com/en/lessons/basics/command-line/). 

Lycka till med din kodning!