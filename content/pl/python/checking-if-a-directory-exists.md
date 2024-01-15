---
title:                "Sprawdzanie, czy istnieje katalog"
html_title:           "Python: Sprawdzanie, czy istnieje katalog"
simple_title:         "Sprawdzanie, czy istnieje katalog"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

W niektórych przypadkach podczas programowania w Pythonie może być konieczne sprawdzenie, czy dany katalog istnieje. To ważne, ponieważ jeśli katalog nie istnieje, program może zwrócić błąd i przerwać działanie, co może utrudnić przetwarzanie dalszych danych.

## Jak to zrobić

```Python
import os

path = "/sciezka/do/katalogu"

if os.path.exists(path):
  print("Katalog istnieje!")
else:
  print("Katalog nie istnieje.")
```

To jest najprostszy sposób sprawdzenia, czy dany katalog istnieje w Pythonie. Wykorzystujemy tutaj moduł `os` i jego funkcję `path.exists()`, która zwraca wartość `True`, jeśli podana ścieżka istnieje, lub `False`, jeśli nie istnieje.

Możemy również wykorzystać funkcję `path.isdir()`, aby dodatkowo sprawdzić, czy podana ścieżka jest katalogiem, a nie plikiem.

```Python
import os

path = "/sciezka/do/katalogu"

if os.path.isdir(path):
  print("To jest katalog!")
else:
  print("To nie jest katalog.")
```

## Zanurzenie w temat

Moduł `os` oferuje wiele innych funkcji związanych z operacjami na plikach i katalogach. Na przykład, możemy wykorzystać funkcję `os.listdir()`, aby uzyskać listę plików i katalogów w danym katalogu, lub `os.path.abspath()`, aby uzyskać bezwzględną ścieżkę do danego pliku lub katalogu.

Możliwości są niemal nieograniczone, więc warto zapoznać się z dokumentacją modułu `os` i eksperymentować z różnymi funkcjami.

## Zobacz również

- [Dokumentacja modułu `os` w Pythonie](https://docs.python.org/3/library/os.html)
- [Przykładowe wykorzystanie modułu `os` w programie](https://realpython.com/python-os-module/)
- [Inne przydatne funkcje do operacji na plikach i katalogach - artykuł w języku polskim](https://techlikepro.com/python/operacje-na-plikach-i-katalogach-w-python-zestaw-funkcji-os/)