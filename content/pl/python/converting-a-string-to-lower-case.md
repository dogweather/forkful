---
title:                "Python: Konwertowanie ciągu znaków na małe litery"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu spotykamy się z koniecznością zmiany tekstu na małe litery. Może to być potrzebne do porównania ciągów znaków, ujednolicenia danych lub wygodniejszego wyświetlania tekstu w aplikacji. W tym artykule dowiecie się jak w prosty sposób przekonwertować string na małe litery za pomocą języka Python.

## Jak to zrobić

Przekonwertowanie tekstu na małe litery w języku Python jest bardzo prostym procesem. Wystarczy użyć metody `lower()` na obiekcie typu string, która zwróci nowy string złożony z małych liter.

```.py
string = "PROGRAMOWANIE W PYTHONIE"
print(string.lower())
# Output: programowanie w pythonie
```

Możemy także użyć tej metody na zmiennych typu string, co pozwala na dowolne manipulacje z tekstem w programie. Przetestujmy to na przykładzie poniższego kodu:

```.py
tekst1 = "HeLLo WoRLD"
tekst2 = "H3LL0 W0RLD"
print(tekst1.lower())
print(tekst2.lower())
# Output: hello world
#         h3ll0 w0rld
```

Jak widać, metoda `lower()` automatycznie konwertuje wszystkie znaki na małe litery, niezależnie od tego czy są one literami czy cyframi.

## Deep Dive

Funkcja `lower()` działa w oparciu o tzw. tabela ASCII, która określa wartości numeryczne dla poszczególnych znaków. Każda litera posiada swoją unikalną wartość, a w przypadku liter z alfabetów nielatynoamerykańskich również mogą występować różnice w kodowaniu.

Innym ważnym aspektem jest fakt, że metoda `lower()` nie zmienia oryginalnego stringa, a jedynie zwraca nowy z konwersją. Dlatego też ważne jest, aby przypisać nowy string do zmiennej, aby móc dalej pracować na przekonwertowanym tekście.

## Zobacz także

- [Tutorial: Ściągawka Python](https://www.python.org/about/gettingstarted/)
- [Dokumentacja języka Python](https://docs.python.org/3/)
- [Kurs programowania w Pythonie na Codecademy](https://www.codecademy.com/learn/learn-python)