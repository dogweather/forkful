---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:37.497298-07:00
description: "I Elixir kalles associative arrays for Maps, og er samlinger av n\xF8\
  kkel-verdi-par der en unik n\xF8kkel peker p\xE5 en verdi. De er veldig praktiske\
  \ for \xE5 lagre\u2026"
lastmod: '2024-03-13T22:44:40.436412-06:00'
model: gpt-4-0125-preview
summary: "I Elixir kalles associative arrays for Maps, og er samlinger av n\xF8kkel-verdi-par\
  \ der en unik n\xF8kkel peker p\xE5 en verdi. De er veldig praktiske for \xE5 lagre\u2026"
title: Bruke associative tabeller
weight: 15
---

## Hva & Hvorfor?

I Elixir kalles associative arrays for Maps, og er samlinger av nøkkel-verdi-par der en unik nøkkel peker på en verdi. De er veldig praktiske for å lagre og hente data på flue, noe som gjør koden din renere og livet ditt enklere.

## Hvordan:

Å opprette et Map er enkelt. Du bruker `%{}` syntaksen, slik:

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

Å få tilgang til verdier gjøres ved å bruke nøklene:

```elixir
IO.puts my_map["name"]
```
Output: `Alex`

For å legge til eller oppdatere verdier, kan du bruke `Map.put/3` funksjonen:

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
Output: `%{"age" => 32, "location" => "NY", "name" => "Alex"}`

Å fjerne nøkler er like enkelt med `Map.delete/2`:

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
Output: `%{"location" => "NY", "name" => "Alex"}`

## Dypdykk

Maps i Elixir er en evolusjon av de eldre nøkkel-verdi lagringstypene, som Hashes i Ruby eller Dictionaries i Python. De tillater mer effektive oppslag og innsettinger, og gjør dem til et førstevalg for moderne Elixir programmering. Det er verdt å merke seg at før Maps, brukte Elixir HashDict og Dict modulene, som nå er avskrevet.

Men, for scenarioer som krever ordnede data, kan du se på nøkkel-lister i Elixir. Disse er lister med tupler, effektive for mindre samlinger, men ikke så ytelsesvennlige for store datasett som Maps.

Husk at Maps lagrer nøklene sine i en "flat" struktur, noe som gjør direkte tilgang til nøstede verdier litt vanskelig. For dyp nesting, kan det være lurt å vurdere strukturert tilgang via `get_in`, `put_in`, `update_in`, og `get_and_update_in` funksjonene, som tillater en mer dynamisk tilnærming til manipulering av nøstede data.

Oppsummert, mens Maps er din gå-til for behov for associative arrays i Elixir, tilbyr språket en rik variasjon av datastrukturer for ethvert scenario, og oppmuntrer deg til å velge det rette verktøyet for jobben.
