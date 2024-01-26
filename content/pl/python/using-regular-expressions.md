---
title:                "Wykorzystanie wyrażeń regularnych"
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Użycie wyrażeń regularnych to przeszukiwanie, edycja i manipulowanie tekstami z użyciem wzorców. Programiści używają ich dla sprawności i szybkości w obsłudze skomplikowanych zadań tekstowych.

## How to:
```Python
import re

# Znajdowanie słów rozpoczynających się od 'p' i kończących na 'e'
pattern = r"p\w+e"
text = "Python to potęga programistyczne przeznaczenie."
matches = re.findall(pattern, text)
print(matches)  # Wynik: ['potęga', 'przeznaczenie']

# Podmiana słów
replaced_text = re.sub(r'\b(p\w+e)\b', 'PYTHON', text)
print(replaced_text)  # Wynik: Python to PYTHON programistyczne PYTHON.
```

## Deep Dive
Wyrażenia regularne (regex) to mocna strona wielu języków programowania już od lat 70-tych. Alternatywą jest użycie metod wbudowanych w stringi (np. `find()`, `replace()`), ale regex daje więcej możliwości i jest często wydajniejszy. Gdy regex jest zaimplementowany w Pythonie poprzez moduł `re`, silnik używa skompilowanej formy wyrażeń, co jeszcze przyspiesza przetwarzanie.

## See Also
- Dokumentacja Pythona o moduł 're': https://docs.python.org/3/library/re.html
- Interactive regex tester and debugger: https://regex101.com/
- Artykuł o wyrażeniach regularnych w Pythonie: https://realpython.com/regex-python/
