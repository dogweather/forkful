---
date: 2024-01-20 17:52:10.489820-07:00
description: "Utskrift av feils\xF8kingsdata handler om \xE5 vise hva som foreg\xE5\
  r under panseret av koden din. Programmerere gj\xF8r dette for \xE5 forst\xE5 flyten\
  \ og oppdage bugs\u2026"
lastmod: '2024-03-13T22:44:40.445988-06:00'
model: gpt-4-1106-preview
summary: "Utskrift av feils\xF8kingsdata handler om \xE5 vise hva som foreg\xE5r under\
  \ panseret av koden din. Programmerere gj\xF8r dette for \xE5 forst\xE5 flyten og\
  \ oppdage bugs\u2026"
title: "Skrive ut feils\xF8kingsdata"
---

{{< edit_this_page >}}

## What & Why?
Utskrift av feilsøkingsdata handler om å vise hva som foregår under panseret av koden din. Programmerere gjør dette for å forstå flyten og oppdage bugs raskt.

## How to:
```Elixir
# Enkel utskrift av en melding
IO.puts "Dette er en debug-melding"

# Debug med inspeksjon av en datastruktur
liste = [1, 2, 3]
IO.inspect liste, label: "Innholdet i listen"

# Bruk av Logger for mer avanserte behov
require Logger
Logger.debug "En mer detaljert debug-melding"
```
Output vil se slik ut:
```
Dette er en debug-melding
Innholdet i listen: [1, 2, 3]
08:45:00.123 [debug] En mer detaljert debug-melding
```

## Deep Dive
I Elixirs tidlige dager var `IO.puts` og `IO.inspect` standardmåter for å skrive ut debug-informasjon. Etter hvert som språket vokste, kom `Logger` modulen, som gir flere nivåer av logging og er integrert med Erlangs :logger-modul. Logger er flott fordi den lar deg kontrollere loggnivået og filtrere ut mindre viktige meldinger i produksjon. `IO.inspect` er fortsatt nyttig for en hurtig titt på variabler, men `Logger` er der du vil gå for et robust loggsystem.

## See Also
- [Elixir's IO module documentation](https://hexdocs.pm/elixir/IO.html)
- [Programming Elixir ≥ 1.6](https://pragprog.com/book/elixir16/programming-elixir-1-6) - En bok for å dykke dypere inn i Elixir.
