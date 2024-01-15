---
title:                "Pisanie do standardowego wyjścia błędu"
html_title:           "Ruby: Pisanie do standardowego wyjścia błędu"
simple_title:         "Pisanie do standardowego wyjścia błędu"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Kiedy piszesz kod, ważne jest aby pamiętać o tzw. "błędach", czyli sytuacjach, w których program może się nie wykonać poprawnie. Jednym z narzędzi, które możesz wykorzystać do obsługi błędów w Ruby, jest standardowe wyjście błędów lub w skrócie `stderr`. Dzięki temu narzędziu możemy otrzymać informacje szczegółowe o błędach, co ułatwia nam debugowanie i poprawianie błędów w kodzie. 

## Jak to zrobić?

Aby pisać do standardowego wyjścia błędów w Ruby, musimy użyć metody `puts` z parametrem `STDERR`, jak w przykładzie poniżej:

```Ruby
puts STDERR, "To jest wiadomość wysłana do standardowego wyjścia błędów."
```
Wynik:

```
To jest wiadomość wysłana do standardowego wyjścia błędów.
```

Możemy również użyć notacji `STDERR.puts` zamiast `puts STDERR`, ale oba sposoby są równoważne i wykonują tę samą funkcję.

## Głębsze wyjaśnienie

W Ruby standardowe wyjście błędów jest obiektem typu IO, który jest dostępny globalnie w całym programie. Oznacza to, że możemy korzystać z tej metody w dowolnym miejscu w naszym kodzie. 

W przypadku, gdy chcemy przekierować błędy do innego miejsca, na przykład do zwykłego wyjścia, możemy użyć metody `warn` zamiast `puts` lub `STDERR.puts`. Przykład:

```Ruby
warn "To jest wiadomość wysłana do zwykłego wyjścia jako ostrzeżenie."
```

Wynik:

```
To jest wiadomość wysłana do zwykłego wyjścia jako ostrzeżenie.
```

Dodatkowo, warto zauważyć, że standardowe wyjście błędów nie tylko służy do wypisywania informacji o błędach, ale także może być wykorzystane do wypisywania dowolnego typu komunikatów lub ostrzeżeń podczas działania programu.

## Zobacz również

- [Dokumentacja Ruby o wyjściu błędów](https://ruby-doc.org/core-3.0.0/IO.html#method-c-stderr)
- [Przewodnik dla początkujących w pisaniu do strumieni w Ruby](https://medium.com/@geebominga/writing-to-streams-in-ruby-f82793f3bea)