---
title:                "Pisanie do standardowego błędu"
html_title:           "Python: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pisanie do standardowego błędu jest popularną techniką, którą programiści wykorzystują do przekazywania informacji o błędach i ostrzeżeniach. Polega to na przesłaniu komunikatu do specjalnego strumienia wyjściowego o nazwie standardowy błąd, zamiast do standardowego strumienia wyjściowego. Programiści wykorzystują tę technikę, aby w łatwy sposób monitorować błędy i w razie potrzeby szybko zareagować.

## Jak to zrobić:

```Python
import sys
sys.stderr.write("Przykładowy błąd")
```

Output:
```
Przykładowy błąd
```

## Schodki w Dół:

- Pisanie do standardowego błędu było popularną techniką w C i C++, ale w Pythonie jest również bardzo użyteczne.
- Alternatywą dla pisania do standardowego błędu jest drukowanie komunikatów do konsoli lub zapisywanie ich do pliku.
- W Pythonie dostępne są dwa strumienie wyjściowe: standardowe wyjście (sys.stdout) i standardowy błąd (sys.stderr).

## Zobacz również:

- [Dokumentacja Pythona o sys.stderr](https://docs.python.org/3/library/sys.html#sys.stderr)
- [Przewodnik po błędach w Pythonie](https://realpython.com/python-exceptions/)
- [Korzystanie ze standardowego błędu w kodzie Python](https://www.bogotobogo.com/python/python_fncs/Python_Errors_Exceptions.php)