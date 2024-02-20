---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:53.682990-07:00
description: "Code organiseren in functies betekent dat gerelateerde operaties worden\
  \ gegroepeerd in herbruikbare blokken. We doen dit om de leesbaarheid en het\u2026"
lastmod: 2024-02-19 22:05:09.558963
model: gpt-4-0125-preview
summary: "Code organiseren in functies betekent dat gerelateerde operaties worden\
  \ gegroepeerd in herbruikbare blokken. We doen dit om de leesbaarheid en het\u2026"
title: Code organiseren in functies
---

{{< edit_this_page >}}

## Wat & Waarom?
Code organiseren in functies betekent dat gerelateerde operaties worden gegroepeerd in herbruikbare blokken. We doen dit om de leesbaarheid en het onderhoud te verbeteren, duplicatie te verminderen en het testen te vereenvoudigen.

## Hoe te:
Laten we een eenvoudige Elixir-functie maken om woorden te kapitaliseren:

```elixir
defmodule StringUtils do
  def capitalize_words(sentence) do
    sentence
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringUtils.capitalize_words("hello elixir world")
```
Output:
```
Hello Elixir World
```
Hier hebben we de logica om woorden te kapitaliseren netjes verpakt in een functie genaamd `capitalize_words`.

## Diepere Duik
In Elixir, en in het bredere Erlang VM ecosysteem, zijn functies burgers van eerste klasse, met het erven van de filosofie om problemen op te splitsen in kleinere, beheersbare en geïsoleerde stukken. Historisch gezien heeft deze functionele aanpak wortels in de lambda-calculus en Lisps, die de filosofie van code als data bevorderen.

Alternatieven voor het organiseren van code kunnen het gebruik van macro's of processen in Elixir zijn voor respectievelijk repetitieve of gelijktijdige taken. Wat betreft de implementatie, Elixir-functies kunnen patroonmatching hanteren en verschillende argumenten ontvangen (arity), wat ze veelzijdig maakt.

## Zie Ook
- [De officiële documentatie van Elixir over functies](https://hexdocs.pm/elixir/Kernel.html#functions)
- [Dave Thomas' "Programming Elixir"](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
