---
title:                "Bruke regulære uttrykk"
aliases:
- /no/elixir/using-regular-expressions.md
date:                  2024-02-03T19:16:35.126616-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke regulære uttrykk"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Regulære uttrykk (regex) i Elixir brukes for å søke, matche og manipulere strenger basert på spesifikke mønstre. Programmerere benytter seg av regex for oppgaver som validering av formater (e-post, URL-er), parsing av logger eller dataekstraksjon, takk være effektiviteten og allsidigheten i behandling av strenger.

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
