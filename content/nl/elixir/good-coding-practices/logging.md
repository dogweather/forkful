---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:57.639297-07:00
description: "Loggen in softwareontwikkeling is de techniek van het vastleggen van\
  \ gebeurtenissen die plaatsvinden terwijl een programma wordt uitgevoerd, meestal\
  \ naar\u2026"
lastmod: 2024-02-19 22:05:09.559963
model: gpt-4-0125-preview
summary: "Loggen in softwareontwikkeling is de techniek van het vastleggen van gebeurtenissen\
  \ die plaatsvinden terwijl een programma wordt uitgevoerd, meestal naar\u2026"
title: Logboekregistratie
---

{{< edit_this_page >}}

## Wat & Waarom?
Loggen in softwareontwikkeling is de techniek van het vastleggen van gebeurtenissen die plaatsvinden terwijl een programma wordt uitgevoerd, meestal naar een bestand of extern systeem. Programmeurs doen dit om inzicht te krijgen in het gedrag van de software, problemen op te lossen en een operationele geschiedenis bij te houden die cruciaal is voor het debuggen en monitoren van de gezondheid van applicaties.

## Hoe te:
In Elixir is de primaire manier om informatie te loggen via de ingebouwde `Logger` module. Zo kun je het gebruiken:

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("Belangrijk proces gestart met param: #{param}")

    # Simuleer dat er werk wordt gedaan
    :timer.sleep(1000)

    Logger.debug("Proces voltooid.")
  rescue
    error -> Logger.error("Er is een fout opgetreden: #{inspect(error)}")
  end
end

# Om je logs te zien, roep je gewoon de functie aan:
MyApplication.do_something_important("MijnParam")
```

Dit eenvoudige fragment laat zien hoe je kunt loggen op verschillende niveaus (`info`, `debug` en `error`). Wanneer je dit uitvoert, zie je het debugbericht niet tenzij je het Logger-niveau configureert naar `:debug`. Standaard filtert Elixirs Logger logberichten onder `:info` uit.

Een voorbeelduitvoer op het `:info` niveau ziet er mogelijk als volgt uit:
```
14:32:40.123 [info]  Belangrijk proces gestart met param: MijnParam
14:32:41.126 [error] Er is een fout opgetreden: %RuntimeError{message: "runtime error"}
```

## Diepgaand:
Elixirs `Logger` is een ingebouwde tool die al vanaf de vroege dagen deel uitmaakt van de taal. Het is beïnvloed door de logsystemen van andere BEAM-talen zoals Erlang. De logger biedt verschillende niveaus van loggen – `:debug`, `:info`, `:warn` en `:error` – en het is inplugbaar, waardoor verschillende backends kunnen worden aangesloten voor het afhandelen van logberichten.

Een alternatief voor de ingebouwde Logger voor meer complexe scenario's is het gebruik van logbibliotheken zoals `Logstash` of `Sentry` voor Elixir, die extra functies kunnen bieden zoals foutopsporing en aggregatie in een meer visueel formaat. Voor lokale ontwikkeling vertrouwen Elixir-ontwikkelaars vaak op de ingebouwde Logger-functionaliteit vanwege de eenvoud en integratie met de BEAM VM.

Onder de motorkap biedt de Logger-module asynchrone en synchrone logmogelijkheden. Asynchroon loggen, dat standaard is, blokkeert de uitvoering van je applicatie niet tijdens het loggen van de berichten. Dit zorgt ervoor dat loggen de prestaties niet negatief beïnvloedt. Echter, synchroon loggen kan worden ingeschakeld voor gevallen waarbij je moet garanderen dat berichten worden gelogd in de volgorde waarin ze zijn verzonden.

De Logger-configuratie kan worden aangepast in het `config/config.exs` bestand van een Elixir-applicatie, waar je het logniveau, formaat, metadata en meer kunt instellen. Vergeet niet om je logniveaus en uitvoer voor verschillende omgevingen aan te passen; je wilt niet dat je productiesystemen overspoeld worden met uitgebreide debuglogs.

## Zie ook:
- De officiële Elixir Logger documentatie: https://hexdocs.pm/logger/Logger.html
- Een blogpost over beste praktijken voor loggen in Elixir: https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Sentry voor Elixir op Hex: https://hex.pm/packages/sentry
- Elixir School's les over Logger: https://elixirschool.com/en/lessons/specifics/debugging/#logging
