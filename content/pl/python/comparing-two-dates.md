---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Porównywanie dwóch dat polega na sprawdzeniu, która z nich jest wcześniejsza, a która późniejsza lub czy są one takie same. Programiści robią to np. aby sortować wydarzenia w kolejności czasowej, weryfikować poprawność zakresów dat czy mierzyć czas pomiędzy dwoma punktami.

## Jak to zrobić:

Python oferuje wbudowany moduł o nazwie `datetime`, który umożliwia prostą manipulację datami i czasem:

```Python
from datetime import datetime
# Ustalamy dwie daty
data1 = datetime(2021, 1, 1)
data2 = datetime(2021, 1, 2)
# Porównujemy daty
print("Czy data1 jest wcześniejsza od data2?", data1 < data2)
```

Na konsole zostanie wyświetlony tekst: `Czy data1 jest wcześniejsza od data2? True`

## Deep Dive

Porównywanie dat to podstawowy element obsługi czasu w wielu językach programowania, włączając w to Pythona. Historycznie ta funkcjonalność była wykorzystywana w różnych aplikacjach, takich jak systemy zarządzania finansami czy nauka maszynowa.

Python oferuje kilka alternatywnych możliwości porównania dat, np. używając operatorów porównania (`==`, `!=`, `<`, `<=`, `>`, `>=`), lub metod takich jak `datetime.equals()` i `datetime.compare()`.

Szczegóły implementacji porównywania dat w Pythonie są dość proste do zrozumienia - Python traktuje daty jako liczby, które są liczone od pewnego punktu startowego, tzw. epoch (1970-01-01 00:00:00).

## Zobacz też:

- Dokumentacja modułu `datetime`: https://docs.python.org/3/library/datetime.html
- Przewodnik po modułach Pythona zw. z datą i czasem: https://realpython.com/python-datetime/
- Moduł `dateutil` do zaawansowanych operacji na datach: https://dateutil.readthedocs.io/en/stable/