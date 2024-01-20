---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å finne lengden på en streng handler om å telle antall tegn i den. Programmerere gjør dette for å manipulere data, sjekke inndata og sammenligne strenger.

## Hvordan å:

Å finne lengden på en streng i Elixir er ganske enkelt. Her er et eksempel:

```elixir
streng = "Hello, Verden!"
IO.puts String.length(streng)
```

Når du kjører denne koden, vil du se `14` fordi det er totalt 14 tegn i strengen "Hello, Verden!".

## Dyp Dykk

Historisk sett ble strenger lagret i datamaskiner som liste av tegn, hvorav hver hadde en tilsvarende numerisk verdi. Å finne lengden på en streng var derfor en prosess med å telle disse verdiene.

I Elixir er det to primære måter å finne lengden på en streng på, `String.length/1` og `byte_size/1`. 'String.length/1' gir antall tegn i en streng, mens `byte_size/1` gir antall bytes. 

For eksempel:

```elixir
streng = "Hei!"
IO.puts String.length(streng)  # Outputs '4'
IO.puts byte_size(streng)      # Outputs '4'
```
Hvis du jobber med strenger som inneholder unicode-tegn, kan antall bytes være annerledes enn antall tegn. Dette skyldes at et enkelt unicode-tegn kan ta opp mer enn en byte.

```elixir
streng = "Hå!"
IO.puts String.length(streng)  # Outputs '3'
IO.puts byte_size(streng)      # Outputs '4'
```
I dette tilfellet er "å" et enkelt tegn, men tar opp to bytes.

## Se Også 

For mer informasjon om hvordan du arbeider med strenger i Elixir, sjekk ut følgende ressurser:

- Elixir's [String module documentation](https://hexdocs.pm/elixir/String.html)
- [Elixir School's guide to Strings](https://elixirschool.com/en/lessons/basics/strings/)