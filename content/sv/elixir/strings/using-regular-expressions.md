---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:39.592562-07:00
description: "Hur man g\xF6r: Elixir anv\xE4nder `Regex`-modulen, som utnyttjar Erlangs\
  \ regex-bibliotek, f\xF6r regex-operationer. H\xE4r \xE4r grundl\xE4ggande anv\xE4\
  ndningsomr\xE5den."
lastmod: '2024-03-13T22:44:37.556938-06:00'
model: gpt-4-0125-preview
summary: "Elixir anv\xE4nder `Regex`-modulen, som utnyttjar Erlangs regex-bibliotek,\
  \ f\xF6r regex-operationer."
title: "Att anv\xE4nda regulj\xE4ra uttryck"
weight: 11
---

## Hur man gör:
Elixir använder `Regex`-modulen, som utnyttjar Erlangs regex-bibliotek, för regex-operationer. Här är grundläggande användningsområden:

```elixir
# Matcha ett mönster - Returnerar den första träffen
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # Utdata: ["hello"]

# Hitta alla matchningar
all_matches = Regex.scan(~r/\d/, "Det finns 2 äpplen och 5 apelsiner.")
IO.inspect(all_matches) # Utdata: [["2"], ["5"]]

# Ersätta delar av en sträng
replaced_string = Regex.replace(~r/\s+/, "Elixir är kul", "_")
IO.inspect(replaced_string) # Utdata: "Elixir_är_kul"
```

För mer komplexa mönster och funktionaliteter kan du överväga att använda tredjepartsbibliotek, även om Elixirs inbyggda `Regex`-modul är ganska kraftfull för de flesta grundläggande sträng- och mönstermatchningsuppgifter.

För att utföra en matchning som inte är känslig för stora eller små bokstäver, använd alternativet `i`:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # Utdata: ["Hello"]
```

Reguljära uttryck kan förkompileras för effektivitet när de används flera gånger:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # Utdata: ["hello"]
```

Elixir stöder också namngivna fångster, vilket kan vara mycket praktiskt för att extrahera specifika delar av en sträng samtidigt som din kod blir mer läsbar:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # Utdata: %{"year" => "2023", "month" => "04", "day" => "15"}
```

Denna korta översikt understryker lättheten med vilken Elixir hanterar reguljära uttryck, vilket möjliggör kraftfulla tekniker för manipulering av strängar och dataextraktion.
