---
date: 2024-01-26 00:51:15.300791-07:00
description: "Obs\u0142uga b\u0142\u0119d\xF3w oznacza pisanie kodu, kt\xF3ry mo\u017C\
  e poradzi\u0107 sobie z sytuacjami, kiedy co\u015B p\xF3jdzie nie tak. Programi\u015B\
  ci robi\u0105 to, aby zapobiec awariom i aby\u2026"
lastmod: '2024-03-13T22:44:35.050835-06:00'
model: gpt-4-1106-preview
summary: "Obs\u0142uga b\u0142\u0119d\xF3w oznacza pisanie kodu, kt\xF3ry mo\u017C\
  e poradzi\u0107 sobie z sytuacjami, kiedy co\u015B p\xF3jdzie nie tak. Programi\u015B\
  ci robi\u0105 to, aby zapobiec awariom i aby\u2026"
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
weight: 16
---

## Co i dlaczego?

Obsługa błędów oznacza pisanie kodu, który może poradzić sobie z sytuacjami, kiedy coś pójdzie nie tak. Programiści robią to, aby zapobiec awariom i aby upewnić się, że ich programy mogą łagodnie się odbudować, gdy zadziała prawo Murphy'ego.

## Jak to zrobić:

W Elixirze często używamy dopasowania wzorców i instrukcji `case` do obsługi różnych wyników, w tym błędów.

```elixir
defmodule Example do
  def divide(a, b) do
    case b do
      0 -> {:error, "Nie można dzielić przez zero."}
      _ -> {:ok, a / b}
    end
  end
end

# Udane dzielenie
{:ok, result} = Example.divide(10, 2)
IO.puts("10 / 2 to #{result}")

# Próba dzielenia przez zero
{:error, reason} = Example.divide(10, 0)
IO.puts("Błąd: #{reason}")
```

Przykładowe wyjście:
```
10 / 2 to 5.0
Błąd: Nie można dzielić przez zero.
```

Gdy uruchomisz ten kod Elixira, otrzymasz albo udane dzielenie, albo komunikat o błędzie, w zależności od wejścia. Tutaj nie ma awarii!

## Pogłębiona analiza

Dawniej obsługa błędów często polegała na sprawdzaniu wartości zwracanych. Jednak z funkcyjnymi korzeniami Elixira mamy dopasowanie wzorców i otagowane krotki, takie jak `{:ok, wartość}` lub `{:error, powód}`, które są bardziej eleganckie.

Istnieją inne sposoby obsługi błędów w Elixerze:

- **Elixir's `try` i `rescue`**, które przypominają tradycyjny `try-catch` w językach imperatywnych, ale są stosowane rzadziej ze względu na preferencję Elixira dla jawności.
- **Supervizorzy i GenSerwery**, które są częścią frameworku OTP Elixira, mają więcej wspólnego z tolerancją na błędy. Obserwują procesy kodu, gotowe je zrestartować, jeśli coś pójdzie źle.

Pod względem implementacji, Elixir buduje na solidności Erlanga. Traktuje błędy jako kolejny typ wiadomości do obsługi z całym dopasowaniem wzorców i dobrocią funkcyjną.

## Zobacz także

Aby dowiedzieć się więcej o obsłudze błędów w Elixerze, sprawdź:

- Oficjalny przewodnik po Elixirze na temat [obsługi błędów](https://elixir-lang.org/getting-started/try-catch-and-rescue.html).
- Dowiedz się więcej o [procesach i OTP](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).
- Forum Elixira jest zawsze dobrym miejscem do zadawania pytań: [https://elixirforum.com](https://elixirforum.com).
