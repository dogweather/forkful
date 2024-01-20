---
title:                "Gör en sträng stor bokstav"
html_title:           "Elixir: Gör en sträng stor bokstav"
simple_title:         "Gör en sträng stor bokstav"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Använda Versaler i en String i Elixir

## Vad & Varför?
Att använda versaler i en textsträng (string) innebär att förvandla tecken till stor bokstav. Programmerare gör det för olika anledningar, som exempelvis program som behöver formell output eller för att jämföra data.

## Hur:
I Elixir gör du det med hjälp av `String.upcase/1` funktionen. Titta på dessa exempel:

```elixir
IO.puts String.upcase("hej världen")
# Output: "HEJ VÄRLDEN"

IO.puts String.upcase("Elixir är roligt")
# Output: "ELIXIR ÄR ROLIGT"
```
Med `String.upcase/1` blir alla små bokstäver omvandlade till stora bokstäver i den angivna strängen.

## Djup Dykning:
Historiskt sett har utvecklare använt versalfunktioner för att standardisera data, särskilt i äldre datorsystem som inte skiljde på små och stora bokstäver. 

Ett alternativ till `String.upcase/1` är `String.capitalize/1` som gör om första bokstaven i varje ord till versal men lämnar resten av strängen som den är. 

```elixir
IO.puts String.capitalize("hej världen")
# Output: "Hej Världen"
```
Implementeringsdetaljer: `String.upcase/1` och `String.capitalize/1` använder Elixirs inbyggda Unicode-stöd för att korrekt hantera en mängd olika tecken från olika språk, inte bara engelska. Detta gör dessa funktioner robusta och tillförlitliga vid internationell användning.

## Se Även:
- Elixir officiella dokumentation om [String.upcase/1](https://hexdocs.pm/elixir/String.html#upcase/1) och [String.capitalize/1](https://hexdocs.pm/elixir/String.html#capitalize/1)
- En bra [tutorial](https://elixircasts.io/string-upcase-and-downcase) om `String.upcase` och `String.downcase` i Elixir
- Artikel om [Elixirs Unicode-stöd](https://www.cogini.com/blog/working-with-unicode-in-elixir/)