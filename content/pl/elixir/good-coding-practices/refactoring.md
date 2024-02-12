---
title:                "Refaktoryzacja"
aliases:
- /pl/elixir/refactoring.md
date:                  2024-01-26T01:18:37.714491-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktoryzacja"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/refactoring.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Refaktoryzacja to proces restrukturyzacji istniejącego kodu bez zmiany jego zewnętrznego zachowania, mający na celu poprawę atrybutów niefunkcjonalnych, takich jak czytelność i możliwość utrzymania. Programiści robią to, aby kod był czystszy, łatwiejszy do zrozumienia i bardziej efektywny, ułatwiając przyszłe aktualizacje i zmniejszając ryzyko błędów.

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
