---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:10.807558-07:00
description: "Jak to zrobi\u0107: W Elixirze mo\u017Cesz u\u017Cy\u0107 funkcji modu\u0142\
  u `IO` takich jak `IO.puts/2` i `IO.warn/2`, aby pisa\u0107 wiadomo\u015Bci do standardowego\
  \ b\u0142\u0119du."
lastmod: '2024-03-13T22:44:35.060832-06:00'
model: gpt-4-0125-preview
summary: "W Elixirze mo\u017Cesz u\u017Cy\u0107 funkcji modu\u0142u `IO` takich jak\
  \ `IO.puts/2` i `IO.warn/2`, aby pisa\u0107 wiadomo\u015Bci do standardowego b\u0142\
  \u0119du."
title: "Pisanie do standardowego b\u0142\u0119du"
weight: 25
---

## Jak to zrobić:
W Elixirze możesz użyć funkcji modułu `IO` takich jak `IO.puts/2` i `IO.warn/2`, aby pisać wiadomości do standardowego błędu:

```elixir
# Pisanie prostej wiadomości do stderr
IO.puts(:stderr, "Błąd: Coś poszło nie tak!")

# Używanie IO.warn, które jest bardziej semantyczne dla ostrzeżeń/błędów
IO.warn("Ostrzeżenie: Zaraz przekroczysz limit!")
```

Przykładowe wyjście w terminalu dla `IO.puts/2`:
```
Błąd: Coś poszło nie tak!
```

Dla `IO.warn/2`, wyjście byłoby podobne, ale `IO.warn/2` jest specjalnie zaprojektowane dla ostrzeżeń i może zawierać dodatkowe formatowanie lub zachowanie w przyszłych wersjach Elixira.

**Używanie bibliotek innych firm**

Chociaż standardowa biblioteka Elixira jest zazwyczaj wystarczająca do obsługi wyjścia błędu standardowego, możesz uznać biblioteki takie jak `Logger` za przydatne do bardziej złożonych aplikacji lub do konfigurowania różnych poziomów i wyjść logów.

Przykład użycia `Loggera` do wypisania komunikatu o błędzie:

```elixir
require Logger

# Konfiguracja Loggera do wypisywania na stderr
Logger.configure_backend(:console, device: :stderr)

# Pisanie wiadomości o błędzie
Logger.error("Błąd: Nie udało się połączyć z bazą danych.")
```

Ta konfiguracja kieruje wyjście `Loggera` specjalnie do stderr, co jest przydatne do oddzielania logowania błędów od standardowych wiadomości logów.
