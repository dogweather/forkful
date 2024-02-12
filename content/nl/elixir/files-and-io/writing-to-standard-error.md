---
title:                "Schrijven naar standaardfout"
aliases:
- /nl/elixir/writing-to-standard-error/
date:                  2024-01-28T22:13:16.228291-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elixir/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar de standaardfout (`stderr`) betekent het uitvoeren van tekst die geen deel uitmaakt van de hoofdprogrammagegevens, maar die fouten of diagnostische informatie aangeeft. Programmeurs doen dit om problemen te debuggen en te loggen zonder de standaarduitvoer (`stdout`) te vervuilen, die vaak gereserveerd is voor de resultaatgegevens van het programma.

## Hoe:

Om naar `stderr` te schrijven in Elixir, gebruik `IO.warn/1` of `IO.puts/2`. Zo werkt het:

```elixir
# Schrijven naar stderr met IO.warn
IO.warn("Er is iets misgegaan!")

# Schrijven naar stderr met IO.puts
IO.puts(:stderr, "Gedetailleerde foutinformatie.")
```

Voorbeelduitvoer naar `stderr`:

```
Er is iets misgegaan!
Gedetailleerde foutinformatie.
```

## Diepere duik

Het historisch gescheiden houden van `stderr` van `stdout` stelde Unix-gebruikers in staat om foutmeldingen duidelijk te onderscheiden van reguliere uitvoer, wat vooral handig kon zijn bij het doorsturen van uitvoer naar een bestand of een ander programma.

Elixir, als een moderne taal, behoudt deze traditie. Hoewel `IO.puts/1` standaard naar `stdout` schrijft, schakelt het doorgeven van de `:stderr` atoom als eerste argument de stroom om. `IO.warn/1` schrijft standaard naar `stderr`, wat geschikt is voor waarschuwingsberichten.

Alternatieven voor foutenlogboekregistratie in Elixir kunnen het Logger-module bevatten voor een meer gestructureerde benadering. Dit kan worden geconfigureerd om logs van verschillende niveaus naar `stderr` te schrijven.

Intern werken de IO-functies van Elixir voor stderr en stdout samen met Erlang's :io module, die op zijn beurt werkt met de onderliggende besturingssysteem I/O-streams.

## Zie ook

- [Documentatie van Elixirs IO-module](https://hexdocs.pm/elixir/IO.html)
- [Documentatie van Elixirs Logger-module](https://hexdocs.pm/logger/Logger.html)
- [Documentatie van Erlangs :io-module](http://erlang.org/doc/man/io.html)
