---
title:                "Python: Pisanie do standardowego błędu"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Gdy pracujesz z programami w języku Python, na pewno już spotkałeś się z pojęciem "standardowego błędu". Jeśli jeszcze nie, nie martw się - w tym artykule wyjaśnimy, czym jest i dlaczego warto umieć z niego korzystać podczas pisania kodu.

## Jak to zrobić

Kiedy chcesz zapisać jakieś komunikaty lub informacje w trakcie działania swojego programu, możesz skorzystać z funkcji `print()`. Jednak jeśli chcesz wyświetlić konkretny błąd lub ostrzeżenie, zamiast `print()` użyjesz funkcji `sys.stderr.write()`. Przykładowy kod wyglądałby mniej więcej tak:

```Python
import sys
sys.stderr.write("To jest błąd programu")
```

Ponadto, jeśli chcesz dodać informacje diagnostyczne do swojego błędu, możesz również skorzystać z `sys.exc_info()`. Przykładowy kod może wyglądać tak:

```Python
try:
    # tutaj znajduje się kod, który może generować błąd
except:
    error_type, error_value, traceback = sys.exc_info()
    sys.stderr.write(f"Błąd typu {error_type.__name__}: {error_value} w linii {traceback.tb_lineno}")
```

Jednym z przydatnych zastosowań pisania do standardowego błędu jest również wyświetlanie postępu w trakcie wykonywania programu. W tym przypadku można skorzystać z funkcji `sys.stderr.flush()` aby wyświetlać aktualny stan programu w tym samym miejscu pamięci, a nie na końcu wiersza.

## Głębszy zanurzenie

Standardowe wyjście i standardowy błąd to dwie różne strumienie, które Python wyświetla w różnych miejscach w terminalu. Standardowe wyjście (`sys.stdout`) jest wyświetlane w wierszu poleceń lub konsoli, natomiast standardowy błąd (`sys.stderr`) wyświetla się na czerwono. Jest to szczególnie przydatne w przypadku dużych projektów, gdzie wiele procesów może generować błędy jednocześnie.

Pamiętaj, aby nie nadużywać funkcji pisania do standardowego błędu, gdyż może to zaciemnić komunikaty istotne dla użytkownika. Ważne jest również, aby przetestować poprawność wyświetlania informacji w standardowym błędzie w różnych systemach operacyjnych, ponieważ niektóre mogą wyświetlać błędy inaczej.

## Zobacz również

- [Dokumentacja funkcji `sys.stderr.write()`](https://docs.python.org/3/library/sys.html#sys.stderr.write)
- [Artykuł o standardowych strumieniach wyjściowych w języku Python](https://realpython.com/python-stdout-stderr/)
- [Przykładowe zastosowanie pisania do standardowego błędu na stronie Stack Overflow](https://stackoverflow.com/questions/60348476/python-output-list-to-stderr-instead-of-stdout)