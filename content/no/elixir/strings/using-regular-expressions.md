---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:35.126616-07:00
description: "Regul\xE6re uttrykk (regex) i Elixir brukes for \xE5 s\xF8ke, matche\
  \ og manipulere strenger basert p\xE5 spesifikke m\xF8nstre. Programmerere benytter\
  \ seg av regex for\u2026"
lastmod: '2024-03-13T22:44:40.433477-06:00'
model: gpt-4-0125-preview
summary: "Regul\xE6re uttrykk (regex) i Elixir brukes for \xE5 s\xF8ke, matche og\
  \ manipulere strenger basert p\xE5 spesifikke m\xF8nstre."
title: "Bruke regul\xE6re uttrykk"
weight: 11
---

## Hvordan:
Elixir benytter `Regex`-modulen, som tar i bruk Erlangs regex-bibliotek, for regex-operasjoner. Her er grunnleggende bruk:

```elixir
# Matcher et mønster - Returnerer det første treffet
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # Utdata: ["hello"]

# Finner alle treff
all_matches = Regex.scan(~r/\d/, "There are 2 apples and 5 oranges.")
IO.inspect(all_matches) # Utdata: [["2"], ["5"]]

# Erstatter deler av en streng
replaced_string = Regex.replace(~r/\s+/, "Elixir is fun", "_")
IO.inspect(replaced_string) # Utdata: "Elixir_is_fun"
```

For mer komplekse mønstre og funksjonaliteter, kan du vurdere å bruke tredjepartsbiblioteker, men for de fleste grunnleggende oppgaver med strenger og mønstersammenligning, er Elixirs innebygde `Regex`-modul ganske kraftig.

For å utføre et ikke-sensitivt treff, bruk `i`-valget:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # Utdata: ["Hello"]
```

Regex-uttrykk kan forhåndskompileres for effektivitet når de brukes flere ganger:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # Utdata: ["hello"]
```

Elixir støtter også navngitte fangster, som kan være svært hendig for å ekstrahere spesifikke deler av en streng samtidig som koden din blir mer lesbar:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # Utdata: %{"year" => "2023", "month" => "04", "day" => "15"}
```

Denne korte oversikten understreker hvor enkelt Elixir håndterer regulære uttrykk, noe som muliggjør kraftige teknikker for manipulering av strenger og dataekstraksjon.
