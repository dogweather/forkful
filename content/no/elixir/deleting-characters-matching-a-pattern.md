---
title:    "Elixir: Sletting av tegn som matcher et mønster"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang ønsket å fjerne visse tegn fra en streng eller et dokument i Elixir? Vel, det er en funksjon som lar deg gjøre nettopp det! Det kan være nyttig i situasjoner der du må foreta en omfattende redigering av en tekst eller når du ønsker å filtrere ut uønskede tegn. I denne artikkelen vil vi utforske hvordan du kan slette tegn som passer til et gitt mønster i Elixir.

## Hvordan gjøre det

For å slette tegn som matcher et mønster, kan du bruke `String.replace` funksjonen. Her er et eksempel som viser hvordan du kan bruke den:

```elixir
str = "Hei, dette er et eksempel 123"
nytt_str = String.replace(str, ~r/\d+/, "")
IO.puts nytt_str

# Output: Hei, dette er et eksempel 
```

I dette eksempelet bruker vi `~r/\d+/` som mønsteret vi vil matche, som betyr at vi fjerner alle numeriske tegn fra strengen. Du kan endre mønsteret etter dine behov for å slette forskjellige typer tegn.

Du kan også bruke regulære uttrykk direkte i mønsteret for å slette mer komplekse mønstre. For eksempel, hvis du vil slette alle tall som kommer etter en punktum i en streng, kan du bruke følgende mønster: `~r/\.\d+/`. Dette vil fjerne alle tall som er etterfulgt av et punktum.

## Dypdykk

Nå som vi har sett på hvordan du kan slette tegn som matcher et gitt mønster, la oss dykke litt dypere inn i funksjonene `String.replace` og regulære uttrykk.

For det første er `String.replace` en del av Elixir `String` modulen, som gir oss mange nyttige funksjoner for å arbeide med strenger. I tillegg til å fjerne tegn som matcher et mønster, kan du også bruke den til å erstatte de matchende tegnene med en annen streng.

Når det gjelder regulære uttrykk, er de et kraftig verktøy for å finne og manipulere tekst basert på et bestemt mønster. Det er verdt å bruke litt tid på å lære grunnleggende om regulære uttrykk for å få mest mulig ut av dem i Elixir.

## Se også

- [Elixir String modulen dokumentasjon](https://hexdocs.pm/elixir/String.html)
- [Regulære uttrykk i Elixir tutorial](https://medium.com/elixir-mix/elixir-tutorial-regular-expressions-88aa9ac503ba)