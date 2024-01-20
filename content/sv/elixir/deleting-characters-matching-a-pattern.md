---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ta bort tecken som matchar ett mönster innebär helt enkelt att du söker och raderar specifika tecken från en sträng på programmets begäran. Programutvecklare gör detta för att rensa upp data, filtrera oönskat innehåll eller för att förenkla sökning av strängar.

## Hur man gör:

Här är ett exempel på hur du kan radera alla förekomster av ett visst tecken i en sträng i Elixir.

```elixir
str = "Hej, Världen!"
IO.puts(String.replace(str, ",", "")) # Detta tar bort alla kommatecken
```
Ovanstående program skriver ut "Hej Världen!" (ingen kommatecken).

## Fördjupning

Historiskt sett uppstod behovet av att ta bort tecken som matchar ett mönster med tillkomsten av databearbetning och textredigering.

Ett alternativ till `String.replace` i Elixir är `Regex.replace`, vilket kan vara användbart när du behöver radera tecken som matchar mer komplexa mönster. Till exempel:

```elixir
str = "Hej! $Underbar $Värld!"
pattern = ~r/\$/
IO.puts(Regex.replace(pattern, str, "")) # Detta tar bort alla dollartecken
```
Detta program skriver ut "Hej! Underbar Värld!" (ingen dollartecken).
    
Det finns flera olika sätt att implementera radering av tecken som matchar ett mönster i Elixir. Oavsett vilken metod du väljer så utnyttjar Elixir sin kraftfulla stränghantering och inbyggda `Matcher`-modul för att göra jobbet.

## Se Även

Sammanfattningsvis, att ta bort tecken som matchar ett visst mönster är en nyttig teknik för att hantera strängdata. Vill du veta mer? Kolla in följande resurser:

- Elixir’s officiella dokumentation på [https://elixir-lang.org/docs.html](https://elixir-lang.org/docs.html)
- Elixir School på [https://elixirschool.com/en/](https://elixirschool.com/en/)
- Erlang Solutions blogg om Elixir programmering på [https://www.erlang-solutions.com/blog.html](https://www.erlang-solutions.com/blog.html)