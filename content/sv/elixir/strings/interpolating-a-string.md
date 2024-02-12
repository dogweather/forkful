---
title:                "Interpolera en sträng"
aliases: - /sv/elixir/interpolating-a-string.md
date:                  2024-01-20T17:50:43.251886-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolera en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
I Elixir använder vi stränginterpolering för att effektivt bygga en sträng med variabler eller uttryck. Programmerare gör detta för att spara tid, öka läsbarheten och underlätta underhåll av koden.

## How to (Hur till)
```elixir
name = "Världen"
greeting = "Hej, #{name}!"
IO.puts greeting
```
Output:
```
Hej, Världen!
```

Mer komplex användning:
```elixir
price = 100
currency = "SEK"
message = "Det totala priset är #{price * 1.25} #{currency} inklusive moms."
IO.puts message
```
Output:
```
Det totala priset är 125.0 SEK inklusive moms.
```

## Deep Dive (Djupdykning)
Interpolering av strängar är inget nytt. De flesta programmeringsspråk har det och i Elixir hanteras det snyggt med #{...}. Alternativ till stränginterpolering innefattar sammanfogning av strängar med `<>` eller att använda `String.concat/1`. Implementeringsmässigt använder Elixir binärer för att representera strängar, vilket gör operationen effektiv. Att interpolera en sträng i Elixir är säkert då det inte finns någon risk för injektionsattacker som med SQL-injektioner.

## See Also (Se även)
- [Elixir Documentation on String Interpolation](https://elixir-lang.org/getting-started/basic-types.html#interpolation)
- [HexDocs for String Module](https://hexdocs.pm/elixir/String.html)
- [String interpolation in Elixir](https://elixir-lang.org/getting-started/basic-types.html)
