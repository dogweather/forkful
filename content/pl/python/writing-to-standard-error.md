---
title:                "Python: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu zdarza się, że chcemy uwzględnić błędy i inne komunikaty w naszym kodzie. Często jest to pomocne przy debugowaniu czy mierzeniu wydajności aplikacji. Pisanie do standardowego wyjścia może być przydatne w tym celu, ale jest jeszcze jedna opcja, którą warto poznać - pisanie do standardowego błędu.

## Jak to zrobić

```Python
import sys

sys.stderr.write("To jest wiadomość błędu")
```

Rezultatem tego kodu będzie wyświetlenie wiadomości na standardowym wyjściu błędu. Można także użyć funkcji `print`, ale musimy wtedy określić, że chcemy pisać do `sys.stderr`:

```Python
import sys

print("To jest wiadomość błędu", file=sys.stderr)
```

## Głębszy zanurzenie

Pisanie do standardowego błędu jest szczególnie przydatne, gdy pracujemy z modułami, które nie posiadają informacji o wyjątkach w standardowym wyjściu. W takich przypadkach możemy użyć `sys.stderr` do wyświetlenia komunikatów o błędach lub informacji diagnostycznych. Możemy także przekierować standardowy błąd do innego pliku za pomocą komendy shellowej `2>`, co pozwoli nam łatwiej analizować wydruki i błędy w naszym kodzie.

## Zobacz także

- [Dokumentacja Python o module sys](https://docs.python.org/3/library/sys.html)
- [Jak Debugować w Pythonie](https://www.codefellows.org/blog/how-to-debug-in-python/)
- [Komunikaty Błędów w Pythonie](https://www.dreamincode.net/forums/topic/293744-error-handling-in-python/)