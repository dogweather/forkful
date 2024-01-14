---
title:    "Elixir: Pisanie do standardowego błędu"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego

Napisanie do standardowego wyjścia błędu może wydawać się niepotrzebnym krokiem w pisaniu programów w języku Elixir, ale w rzeczywistości jest to bardzo przydatna funkcjonalność. Pozwala ona na wyświetlanie błędów i ostrzeżeń bez przerywania działania programu oraz na łatwiejsze debugowanie kodu.

## Jak to zrobić

Aby napisać do standardowego wyjścia błędu, wystarczy użyć funkcji `IO.puts/1` i przekazać jej wiadomość jako argument. Na przykład:

```Elixir
IO.puts("To jest błąd!")
```

W ten sposób zostanie wypisana wiadomość "To jest błąd!" do standardowego wyjścia błędu.

Można również użyć funkcji `IO.inspect/2`, która dodatkowo wyświetli wartość przekazaną jako drugi argument. Na przykład:

```Elixir
IO.inspect("To jest ostrzeżenie!", label: "Ostrzeżenie:")
```

Spowoduje wyświetlenie wiadomości "Ostrzeżenie: To jest ostrzeżenie!" do standardowego wyjścia błędu.

## Głębszy wgląd

Jeśli chcesz mieć większą kontrolę nad wyświetlanymi błędami i ostrzeżeniami, możesz skorzystać z modułu `Logger`. Pozwala on na ustawianie poziomu logowania oraz filtrowanie wiadomości. Można także zdefiniować własne formatowanie wiadomości.

Na przykład, można ustawić poziom logowania na `:warn` oraz filtrować wiadomości, aby wyświetlać tylko te zawierające słowo "ważne":

```Elixir
Logger.configure(level: :warn, filter: fn msg, level ->
  String.contains?(msg, "ważne") && level == :warn
end)
```

## Zobacz także

- [Dokumentacja Elixir - Standardowe wyjścia](https://hexdocs.pm/elixir/IO.html)
- [Dokumentacja Elixir - Moduł Logger](https://hexdocs.pm/logger/Logger.html)
- [Blog Elixir School - Debugowanie w Elixir](https://elixirschool.com/pl/lessons/debugging/debugger/)