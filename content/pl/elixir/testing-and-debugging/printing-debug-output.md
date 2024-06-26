---
date: 2024-01-20 17:52:36.467653-07:00
description: "Jak to zrobi\u0107: Wydruk debugowy pojawi\u0142 si\u0119 w programowaniu\
  \ ju\u017C dawno temu \u2013 by\u0142 jednym z najprostszych metod \u015Bledzenia\
  \ b\u0142\u0119d\xF3w i dzia\u0142ania program\xF3w.\u2026"
lastmod: '2024-04-05T22:50:49.351804-06:00'
model: gpt-4-1106-preview
summary: "Wydruk debugowy pojawi\u0142 si\u0119 w programowaniu ju\u017C dawno temu\
  \ \u2013 by\u0142 jednym z najprostszych metod \u015Bledzenia b\u0142\u0119d\xF3\
  w i dzia\u0142ania program\xF3w."
title: "Drukowanie komunikat\xF3w debugowania"
weight: 33
---

## Jak to zrobić:
```elixir
# Prosty przykład wydruku
IO.puts("To jest debug!")

# Inny przykład z inspekcją struktury danych
lista = [1, 2, 3, 4]
IO.inspect(lista, label: "Sprawdzam zawartość listy")

# Wydruk wartości z funkcji
defmodule Przyklad do
  def policz_sumę(a, b) do
    suma = a + b
    IO.puts("Suma #{a} + #{b} = #{suma}")
    suma
  end
end

Przyklad.policz_sumę(2, 3)
```

Wyniki:
```
To jest debug!
Sprawdzam zawartość listy: [1, 2, 3, 4]
Suma 2 + 3 = 5
```

## Głębsze spojrzenie:
Wydruk debugowy pojawił się w programowaniu już dawno temu – był jednym z najprostszych metod śledzenia błędów i działania programów. Elixir, podobnie jak inne języki, oferuje funkcje takie jak `IO.puts` do szybkiego wyświetlania wartości. 

Istnieją alternatywy dla wydruku debugowego, np. `Logger`, który pozwala na kontrolowanie poziomów logowania i jest bardziej elastyczny do użycia w produkcji. Debug możemy też uruchamiać przy użyciu debuggera IEx (Interactive Elixir), który pozwala na interaktywną pracę z kodem.

Ważne jest, żeby pamiętać o usunięciu wydruków debugowych przed wdrożeniem kodu na serwer czy udostępnieniu go innym – mogą one obniżać wydajność i powodować bałagan w kodzie.

## Zobacz też:
- Dokumentacja IO: https://hexdocs.pm/elixir/IO.html
- Dokumentacja Logger: https://hexdocs.pm/logger/Logger.html
- Debugowanie w IEx: https://hexdocs.pm/iex/IEx.html#module-debugging
