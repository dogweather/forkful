---
date: 2024-01-26 01:10:07.693436-07:00
description: "Jak to zrobi\u0107: Stw\xF3rzmy prost\u0105 funkcj\u0119 w Elixirze\
  \ do kapitalizacji s\u0142\xF3w."
lastmod: '2024-03-13T22:44:35.048910-06:00'
model: gpt-4-1106-preview
summary: "Stw\xF3rzmy prost\u0105 funkcj\u0119 w Elixirze do kapitalizacji s\u0142\
  \xF3w."
title: Organizacja kodu w funkcje
weight: 18
---

## Jak to zrobić:
Stwórzmy prostą funkcję w Elixirze do kapitalizacji słów:

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
Wyjście:
```
Hello Elixir World
```
Tutaj, elegancko spakowaliśmy logikę kapitalizacji słów w funkcję o nazwie `capitalize_words`.

## Dogłębna analiza
W Elixirze, oraz szerszym ekosystemie Erlang VM, funkcje są obywatelami pierwszej klasy, dziedziczą filozofię rozkładania problemów na mniejsze, zarządzalne i izolowane części. Historycznie, to funkcjonalne podejście ma korzenie w lambda kalkulusie i Lispsach, promując filozofię kodu jako danych.

Alternatywy dla organizowania kodu mogą obejmować użycie makr lub procesów w Elixirze do zadań powtarzalnych lub współbieżnych, odpowiednio. Jeśli chodzi o implementację, funkcje w Elixirze mogą obsługiwać dopasowanie wzorców i odbierać różne argumenty (kardynalność), nadając im wszechstronność.

## Zobacz także
- [Oficjalna dokumentacja Elixir na temat funkcji](https://hexdocs.pm/elixir/Kernel.html#functions)
- [Dave Thomas "Programowanie w Elixirze"](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
