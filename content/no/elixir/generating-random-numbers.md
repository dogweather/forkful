---
title:    "Elixir: Generering av tilfeldige tall"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall kan være nyttig i mange forskjellige situasjoner innen Elixir-programmering. Det kan hjelpe deg med å lage tilfeldige input for testing, simulere komplekse situasjoner eller bare legge til en ekstra vri på et spill eller en applikasjon.

## Slik gjør du det

For å generere tilfeldige tall i Elixir, kan du bruke funksjonen `:rand.uniform/1` som tar inn et tall og returnerer et tilfeldig tall mellom 0 og tallet du har gitt. For eksempel:

```Elixir
:rand.uniform(10)
# Output: 6
```

Hvis du vil generere et tilfeldig heltall, kan du bruke `:rand.uniform/2` med en nedre og øvre grense. For eksempel:

```Elixir
:rand.uniform(0, 100)
# Output: 51
```

For å få en liste med tilfeldige tall, kan du bruke `Enum.map/2` sammen med `:rand.uniform/1`. For eksempel:

```Elixir
Enum.map(1..5, fn _ -> :rand.uniform(10) end)
# Output: [2, 9, 6, 4, 1]
```

## Dype dyp

Bak kulissene bruker `:rand.uniform/1` en tilfeldig tallgenerator basert på en algoritme som kalles "Mersenne Twister". Denne har en periode på over 2^19937, noe som betyr at du kan generere et enormt antall tilfeldige tall før du begynner å få tilbake gjentagelser.

Hvis du vil ha mer kontroll over genereringen av tilfeldige tall, kan du bruke `:rand.seed/1` for å sette en spesifikk startverdi for generatoren.

## Se også

- Offisiell Elixir dokumentasjon for `:rand` modulet: https://hexdocs.pm/elixir/1.12/Kernel.SpecialForms.html#%3Arand.uniform/2
- En artikkel om tilfeldige tallgenerering i Elixir: https://medium.com/@yrashk/exploring-randomness-with-elixir-1b2236ec6f8c
- En annen Elixir bloggpost om tilfeldige tall: https://blog.appsignal.com/2021/06/16/generating-random-numbers-in-elixir.html