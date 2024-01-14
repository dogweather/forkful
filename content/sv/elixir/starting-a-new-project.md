---
title:    "Elixir: Att starta ett nytt projekt"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför

Om du är en programmerare som söker en modern, robust och skalbar språk för att starta ett nytt projekt, då är Elixir det perfekta valet för dig. Med ett funktionellt programmeringssätt och stöd för samtidig körning (concurrency), kan Elixir hjälpa dig att bygga snabba och pålitliga applikationer.

## Så här gör man

Det första steget för att starta ett Elixir-projekt är att installera språket på din dator. Det finns flera olika sätt att göra det, men det enklaste är att använda en "version manager" som asdf eller kerl. När du har installerat Elixir kan du skapa ett nytt projekt med kommandot `mix new <projektnamn>`. Detta kommer att skapa en grundstruktur för ditt projekt med nödvändiga filer och mappar.

För att bygga din applikation kan du använda `mix compile` kommandot. Detta kommer att kompilera alla dina Elixir-filer till byte code för att sedan kunna köras.

När ditt projekt är byggt kan du köra det med `mix run` kommandot. Detta kommer att starta din applikation och du kan börja testa och utveckla den.

### Exempel

Här är ett exempel på hur en enkel "Hello World" applikation skulle se ut i Elixir:

```elixir
defmodule Hello do
  def say(msg) do
    IO.puts "Hej " <> msg
  end
end

Hello.say("världen") # Hej världen
```

## Djupdykning

Ett av de största fördelarna med Elixir är dess stöd för samtidighet (concurrency) och distribuerade system. Genom att använda Elixir's inbyggda "actor model", som kallas för "processer", kan du bygga skalbara och tåliga applikationer.

Elixir's standardbibliotek har också ett stort utbud av funktioner och moduler som kan hjälpa dig att bygga applikationer för webben, databaser och mycket mer.

En annan stor fördel med Elixir är dess gemenskap. Det finns ett stort antal resurser online, inklusive handledningar, dokumentation och en aktiv Slack-kanal för att hjälpa dig om du stöter på problem eller behöver råd.

## Se även

- Officiell Elixir-hemsida: https://elixir-lang.org/
- Elixir's dokumentation: https://hexdocs.pm/elixir/
- Elixir Forum: https://elixirforum.com/