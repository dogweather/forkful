---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Python: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Aby uniknąć wprowadzania dużych ilości danych ręcznie podczas uruchamiania programu, warto poznać i zastosować możliwości czytania argumentów wiersza poleceń.

## Jak

Można wykorzystać moduł `sys` i jego funkcję `argv` do odczytania argumentów wiersza poleceń. Zobaczmy przykład:

``` Python
import sys

# Odczytanie argumentów wiersza poleceń
arguments = sys.argv
print("Odczytane argumenty:", arguments)

# Odczytanie kolejnych argumentów poza pierwszym (indeks 0)
print("Pierwszy argument: ", arguments[0])
print("Drugi argument: ", arguments[1])
```

**Wyjście**:

```
Odczytane argumenty: ['script.py', 'arg1', 'arg2']
Pierwszy argument: script.py
Drugi argument: arg1
```

## Deep Dive

Funkcja `argv` zwraca listę argumentów podanych podczas uruchamiania programu. Pierwszym elementem listy zawsze będzie nazwa skryptu, a następnie kolejne argumenty w kolejności, w jakiej zostały wprowadzone. W przypadku braku argumentów, lista będzie zawierać tylko nazwę skryptu.

Możemy również wykorzystać pętlę `for` oraz funkcję `enumerate` do odczytania kolejnych argumentów w linii pętli. Przykład:

``` Python
import sys

# Odczytanie argumentów wiersza poleceń
arguments = sys.argv

# Wykorzystanie pętli for do wypisania kolejnych argumentów
for index, argument in enumerate(arguments):
    print("Argument", index, "to:", argument)
```

**Wyjście**:

```
Argument 0 to: script.py
Argument 1 to: arg1
Argument 2 to: arg2
```

## Zobacz także

- Dokumentacja modułu `sys`: https://docs.python.org/3/library/sys.html
- Przewodnik po linii poleceń Pythona: https://realpython.com/command-line-interfaces-python-argparse/