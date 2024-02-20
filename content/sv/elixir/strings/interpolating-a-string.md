---
date: 2024-01-20 17:50:43.251886-07:00
description: "I Elixir anv\xE4nder vi str\xE4nginterpolering f\xF6r att effektivt\
  \ bygga en str\xE4ng med variabler eller uttryck. Programmerare g\xF6r detta f\xF6\
  r att spara tid, \xF6ka\u2026"
lastmod: 2024-02-19 22:04:56.798456
model: gpt-4-1106-preview
summary: "I Elixir anv\xE4nder vi str\xE4nginterpolering f\xF6r att effektivt bygga\
  \ en str\xE4ng med variabler eller uttryck. Programmerare g\xF6r detta f\xF6r att\
  \ spara tid, \xF6ka\u2026"
title: "Interpolera en str\xE4ng"
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
