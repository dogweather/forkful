---
title:                "Pisanie do standardowego błędu"
html_title:           "Elixir: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisząc do standardowego błędu, programiści raportują błędy, ostrzeżenia lub inne informacje o wykonywaniu programu. Jest to przydatne podczas debugowania i monitorowania działania aplikacji.

## Jak to zrobić:

```Elixir
IO.write(:stderr, "Błąd krytyczny!")
```

Output:
```
Błąd krytyczny!
```

## Głębszy wgląd:

Pisanie do standardowego błędu ma swoje korzenie w tradycji Unixowej, gdzie stanowiło część standardowych protokołów komunikacyjnych. Alternatywą dla tej metody może być wykorzystanie modułu `Logger`, jednakże pisanie do standardowego błędu jest prostszym i szybszym rozwiązaniem. W Elixir, pisanie do standardowego błędu jest obsługiwane przez funkcję `IO.write`.

## Zobacz także:

Dokumentacja Elixir dla `IO.write`: https://hexdocs.pm/elixir/IO.html#write/2