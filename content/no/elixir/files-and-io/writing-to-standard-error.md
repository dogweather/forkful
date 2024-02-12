---
title:                "Skriving til standardfeil"
aliases: - /no/elixir/writing-to-standard-error.md
date:                  2024-02-03T19:32:58.225684-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriving til standardfeil"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive til standardfeil (stderr) i Elixir er en metode for å dirigere feilmeldinger og diagnostikk separat fra hovedutdataen (stdout). Programmerere bruker stderr til å feilsøke og håndtere feil uten å rote til programmets hovedutdata, noe som gjør det enklere å identifisere og løse problemer.

## Hvordan gjøre det:

I Elixir kan du bruke funksjoner i `IO`-modulen som `IO.puts/2` og `IO.warn/2` for å skrive meldinger til standardfeil:

```elixir
# Skrive en enkel melding til stderr
IO.puts(:stderr, "Feil: Noe gikk galt!")

# Bruke IO.warn, som er mer semantisk for advarsler/feil
IO.warn("Advarsel: Du er i ferd med å overskride grensen!")
```

Eksempel på utdata i terminalen for `IO.puts/2`:
```
Feil: Noe gikk galt!
```

For `IO.warn/2`, vil utdataen være lignende, men `IO.warn/2` er spesifikt designet for advarsler og kan inkludere ekstra formatering eller oppførsel i fremtidige Elixir-versjoner.

**Bruke tredjepartsbiblioteker**

Selv om Elixirs standardbibliotek vanligvis er tilstrekkelig for håndtering av standardfeilutdata, kan du finne biblioteker som `Logger` nyttige for mer komplekse applikasjoner eller for å konfigurere forskjellige loggnivåer og utdata.

Eksempel på bruk av `Logger` for å utgi en feilmelding:

```elixir
require Logger

# Konfigurer Logger for å utgi til stderr
Logger.configure_backend(:console, device: :stderr)

# Skrive en feilmelding
Logger.error("Feil: Klarte ikke å koble til databasen.")
```

Denne oppsettet dirigerer `Logger`'s utdata spesifikt til stderr, noe som er nyttig for å skille loggføring av feil fra standard loggmeldinger.
