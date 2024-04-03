---
date: 2024-01-26 01:18:37.714491-07:00
description: "Jak to zrobi\u0107: Uporz\u0105dkujmy wsp\xF3lny wzorzec Elixira. Zrefaktoryzujemy\
  \ funkcj\u0119 `calculate_stats`, kt\xF3ra robi wi\u0119cej ni\u017C powinna, dziel\u0105\
  c j\u0105 na mniejsze,\u2026"
lastmod: '2024-03-13T22:44:35.052130-06:00'
model: gpt-4-0125-preview
summary: "Uporz\u0105dkujmy wsp\xF3lny wzorzec Elixira."
title: Refaktoryzacja
weight: 19
---

## Jak to zrobić:
Uporządkujmy wspólny wzorzec Elixira. Zrefaktoryzujemy funkcję `calculate_stats`, która robi więcej niż powinna, dzieląc ją na mniejsze, wielokrotnego użytku kawałki.

```elixir
defmodule Stats do
  # Oryginalny, nierozbudowany kod
  def calculate_stats(data) do
    total = Enum.sum(data)
    count = Enum.count(data)
    mean = total / count
    {mean, total}
  end
  
  # Zrefaktoryzowany kod
  def calculate_mean(data), do: Enum.sum(data) / Enum.count(data)
  
  def calculate_total(data), do: Enum.sum(data)
  
  def calculate_stats_refactored(data) do
    mean = calculate_mean(data)
    total = calculate_total(data)
    {mean, total}
  end
end

# Przykładowe wyniki
# Przed refaktoryzacją
Stats.calculate_stats([1, 2, 3])
# => {2.0, 6}

# Po refaktoryzacji
Stats.calculate_stats_refactored([1, 2, 3])
# => {2.0, 6}
```
Jak można zauważyć, wynik pozostaje ten sam, ale teraz mamy modułowe funkcje, które można wykorzystać ponownie i testować niezależnie.

## Głębsze zrozumienie
Refaktoryzacja nie jest nowym pojęciem; od wczesnych dni rozwoju oprogramowania jest kluczową częścią programowania. Znaczące prace, takie jak "Refaktoryzacja: Poprawa projektu istniejącego kodu" Martina Fowlera, dostarczają podstawowych praktyk dotyczących refaktoryzacji z wglądami kiedy i jak je stosować.

Alternatywą dla ręcznej refaktoryzacji są narzędzia do automatycznej analizy kodu, które mogą sugerować, a nawet przeprowadzać refaktoryzację. Jednak narzędzia automatyczne mogą nie zawsze pojmować pełny kontekst kodu i mogą przeoczyć subtelności, które zauważyłby recenzent ludzki.

Szczegóły implementacji w Elixirze obejmują zrozumienie paradygmatu funkcjonalnego i wykorzystanie dopasowania wzorców, klauzul strażniczych i operatora pipe do pisania jasnego i zwięzłego kodu. Na przykład, refaktoryzacja często wiąże się z konwertowaniem skomplikowanych funkcji w stylu imperatywnym na mniejsze, komponowalne funkcje, które są zgodne z preferencją Elixira dla niemutowalności i operacji wolnych od efektów ubocznych.

## Zobacz także
Aby dowiedzieć się więcej o technikach refaktoryzacji specyficznych dla Elixira:

- [Oficjalne przewodniki Elixira](https://elixir-lang.org/getting-started/)
- ["Refaktoryzacja: Poprawa projektu istniejącego kodu" autorstwa Martina Fowlera](https://martinfowler.com/books/refactoring.html), dla ogólnych zasad, które można zastosować w Elixirze.
- [Credo, narzędzie do statycznej analizy kodu dla Elixira](https://github.com/rrrene/credo), które promuje najlepsze praktyki.
- [Ścieżka Elixira na Exercism](https://exercism.org/tracks/elixir), dla praktycznych ćwiczeń, które często dotyczą refaktoryzacji.
