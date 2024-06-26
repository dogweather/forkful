---
date: 2024-01-20 17:52:10.489820-07:00
description: "How to: I Elixirs tidlige dager var `IO.puts` og `IO.inspect` standardm\xE5\
  ter for \xE5 skrive ut debug-informasjon. Etter hvert som spr\xE5ket vokste, kom\u2026"
lastmod: '2024-04-05T22:50:54.457025-06:00'
model: gpt-4-1106-preview
summary: "I Elixirs tidlige dager var `IO.puts` og `IO.inspect` standardm\xE5ter for\
  \ \xE5 skrive ut debug-informasjon."
title: "Skrive ut feils\xF8kingsdata"
weight: 33
---

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
