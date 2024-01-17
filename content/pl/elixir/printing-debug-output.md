---
title:                "Wydrukowanie informacji debugowania"
html_title:           "Elixir: Wydrukowanie informacji debugowania"
simple_title:         "Wydrukowanie informacji debugowania"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Wypisywanie wyjścia debugowania jest procesem, w którym programista wysyła informacje o swoim kodzie do terminala w celu zrozumienia, co się dzieje podczas jego wykonywania. Jest to szczególnie przydatne w przypadku naprawiania błędów i śledzenia jakichkolwiek problemów z wykonywanym programem.

## Jak to zrobić:

```Elixir
IO.inspect("Hello, world!")
```

Output:

```Elixir
"Hello, world!"
```

Więcej przykładów możesz znaleźć na stronach dokumentacji Elixir.

## Głębsze wyjaśnienia:

Wypisywanie wyjścia debugowania stało się standardem w programowaniu, aby pomóc programistom w zrozumieniu swojego kodu i naprawieniu błędów. Alternatywnymi metodami są na przykład użycie narzędzi do debugowania, takich jak debugger, lub stosowanie wbudowanych funkcji takich jak `IO.inspect`. Elixir zapewnia wiele wbudowanych funkcji, które mogą pomóc w wypisywaniu debug output, takich jak `IO.inspect`, `IO.puts` i `IO.warn`.

## Zobacz także:

- Dokumentacja Elixir: https://hexdocs.pm/elixir/Kernel.html#inspect/2
- Narzędzia do debugowania Elixir: https://hexdocs.pm/iex/IEx.html#debugger/3