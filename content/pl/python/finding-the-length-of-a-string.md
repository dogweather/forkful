---
title:                "Python: Znalezienie długości ciągu znaków"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego warto obliczyć długość łańcucha w Pythonie

Obliczanie długości łańcucha jest niezwykle przydatnym narzędziem w Pythonie. Wiele programów wymaga manipulowania tekstami, a do tego potrzebna jest znajomość długości łańcucha. Nawet jeśli nie używasz Pythona w swojej codziennej pracy, poznaj tę funkcję - może przydać się w przyszłości.

## Jak obliczyć długość łańcucha w Pythonie

Aby obliczyć długość łańcucha w Pythonie, użyj funkcji `len()`. Przykładowe użycie wygląda następująco:

```Python
string = "To jest przykładowy tekst"
print(len(string))

# Output: 24
```

Mamy tutaj łańcuch o długości 24 znaków, a korzystając z funkcji `len()`, możemy to łatwo sprawdzić.

Możemy również obliczyć długość łańcucha, w którym znajdują się spacje i inne znaki specjalne:

```Python
string = "Ala ma kota 123 !@#"
print(len(string))

# Output: 19
```

Funkcja `len()` liczy wszystkie znaki w łańcuchu, w tym spacje, znaki specjalne i liczby.

## Głębszy wgląd w obliczanie długości łańcucha

W Pythonie łańcuchy są reprezentowane jako tablice (listy) znaków. Oznacza to, że możemy uzyskać długość łańcucha za pomocą funkcji `len()` tak jak w przypadku innych tablic. Funkcja ta zwraca liczbę elementów w danej tablicy, a w przypadku łańcucha jest to liczba znaków.

Warto również zauważyć, że funkcja `len()` jest uniwersalna i można jej użyć nie tylko do łańcuchów, ale również do list, słowników, krotek itp.

## Zobacz również

- [Dokumentacja Pythona na temat funkcji `len()`](https://docs.python.org/3/library/functions.html#len)
- [Rozwiązanie problemu z obliczaniem długości łańcucha w Pythonie](https://www.geeksforgeeks.org/python-string-length-len/)
- [Porównanie różnych sposobów obliczania długości łańcucha w Pythonie](https://stackoverflow.com/questions/1712227/how-do-i-get-the-number-of-elements-in-a-list)