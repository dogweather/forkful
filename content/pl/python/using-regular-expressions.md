---
title:                "Używanie wyrażeń regularnych"
html_title:           "Python: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego?

Wykorzystywanie wyrażeń regularnych to podstawowa umiejętność, którą powinien posiadać każdy programista w Pythonie. Pozwala ona na efektywne przeszukiwanie i manipulowanie tekstem, co jest bardzo przydatne w wielu sytuacjach. Dzięki wykorzystaniu wyrażeń regularnych, można łatwo znaleźć lub zastąpić określone wzorce w tekście, bez konieczności ręcznego przeszukiwania całego dokumentu.

## Jak to zrobić?

Aby używać wyrażeń regularnych w Pythonie, należy zaimportować moduł `re`. Następnie można użyć funkcji `search()` lub `match()` do znalezienia dopasowań do danego wyrażenia. Na przykład, jeśli chcemy wyszukać w tekście wszystkie liczby całkowite, możemy użyć wyrażenia `r'\d+'`.

```Python
import re

text = 'Lorem ipsum 123 dolor sit amet 456'

matches = re.search(r'\d+', text)

print(matches.group())
# Output: 123
```

## Głębsze zanurzenie

Wyrażenia regularne zostały pierwotnie opracowane przez naukowców w celu analizy tekstów. Obecnie są one powszechnie wykorzystywane we wszystkich językach programowania, a także w programach do przetwarzania tekstu, takich jak edytory tekstu czyarkusze kalkulacyjne.

Alternatywami dla wyrażeń regularnych w Pythonie są między innymi moduły `fnmatch`, `glob` czy `pathlib`, jednak nie oferują one takiej elastyczności i możliwości jak wyrażenia regularne.

Implementacja wyrażeń regularnych w Pythonie jest oparta na wykorzystaniu metaznaków, czyli specjalnych symboli, które pozwalają na definiowanie wzorców do dopasowywania. Należy pamiętać, że wyrażenia regularne są wrażliwe na wielkość liter, więc należy zachować ostrożność przy tworzeniu wzorców.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o wyrażeniach regularnych w Pythonie, możesz skorzystać z poniższych źródeł:

- [Dokumentacja modułu `re` w Pythonie](https://docs.python.org/3/library/re.html)
- [Tutorial o wyrażeniach regularnych w Pythonie](https://www.w3schools.com/python/python_regex.asp)
- [Przewodnik po wyrażeniach regularnych w Pythonie z przykładami](https://realpython.com/regex-python/)