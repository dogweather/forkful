---
title:                "Extrahera delsträngar"
date:                  2024-01-20T17:45:34.738211-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extrahera delsträngar"

category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera delsträngar innebär att man plockar ut specifika delar av en sträng. Programmerare gör detta för att bearbeta eller analysera specifika bitar av information.

## Hur man gör:
```elixir
# Starta med en grundsträng
original = "Hej världen! Hur mår du idag?"

# Extrahera en delsträng med String.slice/3
delsträng = String.slice(original, 4, 7)
IO.puts(delsträng) # => "världen"

# Använda negativa index för att börja från slutet
delsträng_bak = String.slice(original, -9, 7)
IO.puts(delsträng_bak) # => "Hur mår"

# Få delsträng med en startindex och längd
delsträng_start = String.slice(original, 10..15)
IO.puts(delsträng_start) # => "Hur mår"
```

## Fördjupning
Delsträngsextraktion är grundläggande i de flesta programmeringsspråk, inklusive Elixir. Historiskt sett härstammar det från behovet av textbehandling. Alternativ till `String.slice` i Elixir kan vara användning av regex med `Regex.run/2` eller att iterera över strängen med `String.next_codepoint/1`.

Exempel på alternativ:
```elixir
# Använda Regex för att hitta matchande text
match = Regex.run(~r/Hur mår/, original)
IO.inspect(match) # => ["Hur mår"]

# Iterera över sträng och välja ut delar
{delsträng_iter, _} = original |> String.next_codepoint()
IO.puts(delsträng_iter) # => "H"
```

Elixir använder UTF-8 teckenkodning vilket innebär att `String.slice/3` hanterar Unicode-strängar korrekt, till skillnad från äldre språk där substrings kan bryta tecken.

## Se även
- Elixir's `String`-modul dokumentation: https://hexdocs.pm/elixir/String.html
- Elixir School's stränglektion: https://elixirschool.com/en/lessons/basics/strings/
- Översikt av Unicode i Elixir: https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html
