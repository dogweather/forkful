---
title:                "Elixir: Pisanie do standardowego wyjścia błędów"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego pisać do standardowego błędu w Elixirze?

Pisanie do standardowego błędu (lub stderr) jest istotną częścią tworzenia oprogramowania w Elixirze. Jest to przydatne narzędzie do debugowania i przechwytywania błędów w naszych programach. Dzięki temu możemy szybciej znaleźć i naprawić potencjalne problemy w naszym kodzie.

## Jak to zrobić?

Poniżej znajdują się przykłady kodu i wyjścia, które pokazują, jak pisać do stderr w Elixirze.

```
Elixir a = 1/0
** (ArithmeticError) bad arror in arithmetic expression: a = 1/0
```
W powyższym przykładzie używamy błędu arytmetycznego, aby wyświetlić błąd w konsoli. Możemy również korzystać z funkcji ```IO.write/2``` i ```IO.puts/2``` do wyświetlenia wiadomości w terminalu.

```
IO.write(:stderr, "To jest błąd\n")
To jest błąd
:ok
```
Możemy również używać stderr wewnątrz wyjątków, aby przechwytywać i logować błędy w naszym kodzie.

```
try do
  raise "To jest błąd"
rescue
  e in Exception ->
    IO.write(:stderr, "#{try e.why rescue nil}\n")
end
To jest błąd
:ok
```

## Dogłębny przegląd

Istnieje wiele przydatnych funkcji wbudowanych w Elixir, które pomagają nam w obsłudze i wyświetlaniu błędów w naszych programach. Możemy również używać modułu `Logger` do logowania błędów do plików lub do wysyłania powiadomień do naszych systemów monitoringu.

Pamiętaj, aby używać zamienników do standardowego wyjścia, takich jak stderr, zamiast prostego drukowania do konsoli. W przeciwnym razie możemy stracić ważne informacje lub niezamierzonych efektów ubocznych.

## Zobacz także

- [Dokumentacja Elixir - Debugowanie i obsługa błędów](https://elixir-lang.org/getting-started/debugging.html)
- [Książka - "Elixir w akcji"](https://www.manning.com/books/elixir-in-action)
- [Blog - "Wyjątki, błędy i debugger w Elixirze"](https://www.mojdigital.pl/2017/09/exeption-try-catch-debugger-elsixir/)