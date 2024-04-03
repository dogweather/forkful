---
date: 2024-01-26 01:02:50.015063-07:00
description: "Loggning inom mjukvaruutveckling \xE4r tekniken att registrera h\xE4\
  ndelser som intr\xE4ffar n\xE4r ett program k\xF6rs, vanligtvis till en fil eller\
  \ externt system.\u2026"
lastmod: '2024-03-13T22:44:37.573859-06:00'
model: gpt-4-1106-preview
summary: "Loggning inom mjukvaruutveckling \xE4r tekniken att registrera h\xE4ndelser\
  \ som intr\xE4ffar n\xE4r ett program k\xF6rs, vanligtvis till en fil eller externt\
  \ system."
title: Loggning
weight: 17
---

## Vad & Varför?
Loggning inom mjukvaruutveckling är tekniken att registrera händelser som inträffar när ett program körs, vanligtvis till en fil eller externt system. Programmerare gör detta för att få insikter i mjukvarans beteende, felsöka problem och bibehålla en historik av operativ historik som är avgörande för felsökning och övervakning av applikationernas hälsa.

## Hur man gör:
I Elixir är det primära sättet att logga information genom den inbyggda `Logger`-modulen. Så här kan du använda den:

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("Startar viktig process med param: #{param}")

    # Simulerar arbete som utförs
    :timer.sleep(1000)

    Logger.debug("Processen slutförd.")
  rescue
    error -> Logger.error("Ett fel inträffade: #{inspect(error)}")
  end
end

# För att se dina loggar, bara anropa funktionen:
MyApplication.do_something_important("MyParam")
```

Detta enkla kodsnutt visar hur man loggar på olika nivåer (`info`, `debug`, och `error`). När du kör detta kommer du inte se debug-meddelandet om du inte konfigurerar Logger-nivån till `:debug`. Som standard filtrerar Elixirs Logger bort loggmeddelanden under `:info`.

Exempel på utdata på `:info`-nivå kan se ut så här:
```
14:32:40.123 [info]  Startar viktig process med param: MyParam
14:32:41.126 [error] Ett fel inträffade: %RuntimeError{message: "runtime error"}
```

## Fördjupning:
Elixirs `Logger` är ett inbyggt verktyg som har varit en del av språket sedan dess tidiga dagar. Det är påverkat av loggsystemen från andra BEAM-språk som Erlang. Loggern erbjuder olika nivåer av loggning – `:debug`, `:info`, `:warn`, och `:error` – och den är utbyggbar, vilket tillåter olika bakändar att kopplas in för hantering av loggmeddelanden.

Ett alternativ till den inbyggda Loggern för mer komplexa scenarier är användning av loggningsbibliotek såsom `Logstash` eller `Sentry` för Elixir, vilka kan tillhandahålla ytterligare funktioner som spårning och aggregering av fel i ett mer visuellt format. För lokal utveckling förlitar Elixir-utvecklare sig ofta på den inbyggda Logger-funktionaliteten för dess enkelhet och integration med BEAM-maskinvaran (VM).

Under ytan erbjuder Logger-modulen asynkron och synkron loggning. Asynkron loggning, som är standard, blockerar inte exekveringen av din applikation vid loggning av meddelanden. Detta garanterar att loggning inte negativt påverkar prestanda. Men synkron loggning kan aktiveras för fall där du behöver garantera att meddelanden loggas i den ordning de skickades.

Logger-konfigurationen kan justeras i `config/config.exs`-filen för en Elixir-applikation, där du kan ställa in loggningsnivå, format, metadata och mer. Kom alltid ihåg att justera dina loggningsnivåer och utdata för olika miljöer; du skulle inte vilja att verboserings-debug-loggar översvämmar dina produktionssystem.

## Se även:
- Den officiella Elixir Logger-dokumentationen: https://hexdocs.pm/logger/Logger.html
- Ett blogginlägg om bästa praxis för loggning i Elixir: https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Sentry för Elixir på Hex: https://hex.pm/packages/sentry
- Elixir Schools lektion om Logger: https://elixirschool.com/en/lessons/specifics/debugging/#logging
