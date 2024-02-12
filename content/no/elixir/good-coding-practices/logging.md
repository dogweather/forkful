---
title:                "Loggføring"
aliases:
- /no/elixir/logging.md
date:                  2024-01-26T01:02:43.657794-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggføring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/logging.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Logging i programvareutvikling er teknikken for å registrere hendelser som finner sted mens et program kjører, vanligvis til en fil eller et eksternt system. Programmerere gjør dette for å få innsikt i programvarens oppførsel, feilsøke problemer, og vedlikeholde en historikk over operasjonsloggen som er kritisk for feilsøking og overvåkning av applikasjonenes helse.

## Hvordan:
I Elixir er den primære måten å logge informasjon på gjennom det innebygde `Logger`-modulen. Slik kan du bruke den:

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("Starter viktig prosess med parameter: #{param}")

    # Simulerer arbeid som utføres
    :timer.sleep(1000)

    Logger.debug("Prosess fullført.")
  rescue
    error -> Logger.error("En feil oppstod: #{inspect(error)}")
  end
end

# For å se loggene dine, bare kaller du funksjonen:
MyApplication.do_something_important("MyParam")
```

Dette enkle kodeutdraget viser hvordan du logger på forskjellige nivåer (`info`, `debug` og `error`). Når du kjører dette, vil du ikke se debug-meldingen med mindre du konfigurerer Logger-nivået til `:debug`. Som standard filtrerer Elixirs `Logger` ut loggmeldinger under `:info`.

Eksempelutskrift på `:info`-nivået kan se slik ut:
```
14:32:40.123 [info]  Starter viktig prosess med parameter: MyParam
14:32:41.126 [error] En feil oppstod: %RuntimeError{message: "kjøretidsfeil"}
```

## Dypdykk:
Elixirs `Logger` er et innebygd verktøy som har vært en del av språket siden de tidlige dagene. Den er påvirket av loggesystemene fra andre BEAM-språk som Erlang. Loggeren tilbyr forskjellige loggenivåer – `:debug`, `:info`, `:warn` og `:error` – og den er pluggbar, noe som tillater at forskjellige backend-løsninger kan kobles til for håndtering av loggmeldinger.

Ett alternativ til den innebygde Loggeren for mer komplekse scenarioer er bruken av loggebiblioteker som `Logstash` eller `Sentry` for Elixir, som kan tilby ytterligere funksjoner som feilsporing og samling i et mer visuelt format. For lokal utvikling stoler Elixir-utviklere ofte på innebygd Logger-funksjonalitet for dens enkelhet og integrasjon med BEAM VM.

Under panseret tilbyr Logger-modulen asynkron og synkron logging. Asynkron logging, som er standard, blokkerer ikke utførelsen av applikasjonen din mens den logger meldingene. Dette sikrer at logging ikke negativt påvirker ytelsen. Men synkron logging kan aktiveres for tilfeller hvor du må garantere at meldinger logges i den rekkefølgen de ble sendt.

Logger-konfigurasjonen kan justeres i `config/config.exs`-filen i en Elixir-applikasjon, hvor du kan sette loggenivået, formatet, metadata og mer. Husk alltid å justere loggenivåene og utskriftene for forskjellige miljøer; du vil ikke ha omfattende debug-loggføringer som flommer over produksjonssystemene dine.

## Se Også:
- Den offisielle Elixir Logger-dokumentasjonen: https://hexdocs.pm/logger/Logger.html
- Et blogginnlegg om beste praksis for logging i Elixir: https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Sentry for Elixir på Hex: https://hex.pm/packages/sentry
- Elixir School's leksjon om Logger: https://elixirschool.com/en/lessons/specifics/debugging/#logging
