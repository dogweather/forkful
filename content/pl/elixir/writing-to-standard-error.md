---
title:                "Pisanie do standardowego błędu"
date:                  2024-01-19
simple_title:         "Pisanie do standardowego błędu"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Pisanie do standardowego błędu (stderr) pozwala oddzielić komunikaty o błędach od normalnego wyjścia programu. Ułatwia to debugowanie i logowanie, ponieważ błędy można przekierować i przetworzyć osobno.

## Jak to zrobić:
W Elixirze możesz pisać do stderr używając modułu `IO` i funkcji `puts/2` z argumentem `:stderr`.

```elixir
# Wysyłanie komunikatu do stderr
IO.puts(:stderr, "Wystąpił błąd!")

# Przykładowe wyjście: Wystąpił błąd!
```

Jeżeli chcesz użyć bardziej zaawansowanych funkcji, jak formatowanie, użyj `IO.warn/2`.

```elixir
# Wysyłanie sformatowanego komunikatu do stderr
IO.warn("To jest ostrzeżenie z parametrem: %{param}", param: "wartość")

# Przykładowe wyjście: To jest ostrzeżenie z parametrem: wartość
```

## Głębsze spojrzenie:
W systemach Uniksowych, stdout i stderr to dwa osobne kanały wyjściowe; stdout jest dla normalnego wyjścia programu, a stderr dla błędów i ostrzeżeń. To rozdzielenie pozwala na łatwe przekierowanie tych strumieni do innych programów lub plików. W Elixirze, wszystko jest obsługiwane przez Moduł `IO`, który zapewnia interakcję z terminalami używając różnych funkcji jak `puts/2` czy `warn/2`. Historia stderr sięga pierwszych dni Uniksa i koncepcji strumieni danych.

Alternatywnie, można użyć niskopoziomowych funkcji z Erlanga, na przykład `:erlang.display/1`, ale to nie jest zalecane do standardowego wykorzystania i służy głównie do debugowania VM.

## Zobacz także:
- Dokumentacja `IO` w Elixirze: https://hexdocs.pm/elixir/IO.html
- Artykuł o strumieniach wejścia/wyjścia w Uniksie: https://en.wikipedia.org/wiki/Standard_streams
- Przydatny przewodnik o przekierowaniu strumieni w systemie Uniks: https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-3.html
