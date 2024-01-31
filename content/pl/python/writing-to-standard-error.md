---
title:                "Pisanie do standardowego błędu"
date:                  2024-01-19
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"

category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Pisanie do standardowego wyjścia błędów (stderr) umożliwia odseparowanie komunikatów o błędach od regularnego wyniku programu. Robimy to, aby ułatwić debugowanie i logowanie, umożliwiając monitorowanie błędów niezależnie od normalnego wyjścia.

## Jak to zrobić:
```python
import sys

print("To jest zwykła wiadomość.")
sys.stderr.write("To jest wiadomość błędu.\n")

# Alternatywa
print("Inny błąd", file=sys.stderr)
```
Output:
```
To jest zwykła wiadomość.
To jest wiadomość błędu.
Inny błąd
```

## Głębsza wiedza
Pisanie do stderr istnieje od początków UNIKSa i jest standardem w wielu językach programowania. Alternatywy to użycie logging frameworków, które mogą kierować błędy do różnych miejsc, np. plików czy systemów monitorowania. W Pythonie, `print` można przekierować do `sys.stderr` używając argumentu `file`, co pozwala na bardziej zwięzłą składnię porównując do `sys.stderr.write()`.

## Zobacz także
- Dokumentacja Pythona na `sys.stderr`: https://docs.python.org/3/library/sys.html#sys.stderr
- Moduł `logging` Pythona dla zaawansowanego logowania: https://docs.python.org/3/library/logging.html
- Artykuł o obsłudze błędów w Pythonie: https://realpython.com/python-exceptions/
