---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:57.103691-07:00
description: "Reguliere expressies (regex) zijn patronen die gebruikt worden om te\
  \ zoeken naar combinaties van karakters in tekst. Programmeurs gebruiken ze voor\
  \ taken\u2026"
lastmod: '2024-03-13T22:44:50.451951-06:00'
model: gpt-4-0125-preview
summary: Reguliere expressies (regex) zijn patronen die gebruikt worden om te zoeken
  naar combinaties van karakters in tekst.
title: Reguliere expressies gebruiken
weight: 11
---

## Hoe te:
In Elixir gebruik je regex met ingebouwde patronen of door je eigen te maken met de `Regex` module. Hier is een snel voorbeeld:

```elixir
# Zoeken naar het woord "hello"
regex = ~r/hello/
"hello world" =~ regex
# => true

# Zoeken zonder op hoofdletters te letten
regex = ~r/hello/i
"Hello world" =~ regex
# => true

# "world" vervangen door "Elixir"
"hello world" |> String.replace(~r/world/, "Elixir")
# => "hello Elixir"
```

## Diepere Duik
Regex werd in de jaren 50 gepioneer door wiskundige Stephen Kleene. Elixir implementeert regex via de PCRE (Perl Compatible Regular Expressions) bibliotheek, die patronen robuust matcht. Alternatieven zoals string matching met `String.contains?/2` of `String.starts_with?/2` bestaan, maar ze missen de flexibiliteit die regex biedt. Elixir's `Regex` module compileert patronen naar een intern formaat geoptimaliseerd voor herhaaldelijk gebruik, wat rekentijd bespaart.

## Zie Ook
- Documentatie van Elixir's `Regex` module: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- Regex101, een online regex tester en debugger: [https://regex101.com/](https://regex101.com/)
- "Programming Elixir" door Dave Thomas - een uitgebreide gids die ook het gebruik van regex behandelt.
