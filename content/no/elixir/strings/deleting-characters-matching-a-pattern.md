---
date: 2024-01-20 17:42:13.446821-07:00
description: "Hvordan: Elixir bruker regex, eller regul\xE6re uttrykk, for \xE5 identifisere\
  \ tegnm\xF8nstre. Regex har sine r\xF8tter i teoretisk informatikk og ble popul\xE6\
  rt i\u2026"
lastmod: '2024-04-05T21:53:41.408051-06:00'
model: gpt-4-1106-preview
summary: "Elixir bruker regex, eller regul\xE6re uttrykk, for \xE5 identifisere tegnm\xF8\
  nstre."
title: "Slette tegn som matcher et m\xF8nster"
weight: 5
---

## Hvordan:
```elixir
# Med Regex.replace/3 kan vi enkelt slette tegn som matcher et mønster

# Slette alle ikke-numeriske tegn fra en streng
slettet_pattern = Regex.replace(~r/[^\d]/, "Telefon: 123-456-7890", "")
IO.puts(slettet_pattern) # => "1234567890"

# Slette alle vokaler fra en streng
slettet_vokaler = Regex.replace(~r/[aeiouyæøå]/i, "Heisann, dette er en test", "")
IO.puts(slettet_vokaler)  # => "Hsnn, dtt r n tst"
```

## Deep Dive
Elixir bruker regex, eller regulære uttrykk, for å identifisere tegnmønstre. Regex har sine røtter i teoretisk informatikk og ble populært i Unix-verktøy på 70-tallet. Det gjør tekstmanipulasjon kraftig og fleksibelt.

Alternativt kan du bruke String-funksjoner som `String.replace/3`, men de er begrenset til enkle erstatninger. Regex gir mer avanserte mønstre.

Implementasjonsdetaljer for sletting med mønster kommer ned til å definere det riktige uttrykket som matcher tegnene som skal fjernes og anvende det gjennom Regex-modulen i Elixir. Effektiviteten av dette avhenger av kompleksiteten til mønsteret og lengden på strengen.

## Se Også
- Elixir Regex modul: [Elixir Regex Documentation](https://hexdocs.pm/elixir/Regex.html)
- Regex grunnleggende: [Regular-Expressions.info](https://www.regular-expressions.info/)
- String manipulasjon i Elixir: [Elixir String Documentation](https://hexdocs.pm/elixir/String.html)
