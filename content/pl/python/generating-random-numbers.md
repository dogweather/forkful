---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Generowanie liczb losowych to proces tworzenia ciągu liczb, które są nieprzewidywalne i nie powtarzają się w regularny sposób. Programiści generują liczby losowe aby generować dane testowe, symulować różne scenariusze, do gier komputerowych czy dla kodów jednorazowego użytku (OTP).

## Jak to zrobić:

Biblioteka "random" w Pythonie umożliwia generowanie liczb losowych, oto przykładowy kod:

```python
import random

# Generuje losową liczbę zmiennoprzecinkową między 0 i 1
random_number = random.random()
print(random_number)

# Losuje liczbę całkowitą z zakresu (włącznie)
random_integer = random.randint(1, 10)
print(random_integer)

# Losuje element z listy
random_element = random.choice(['jabłko', 'banan', 'cytryna'])
print(random_element)
```

## Pogłębione informacje:

Historia generowania liczb losowych sięga lat 40., kiedy John von Neumann wprowadził "metodę środkowego kwadratu". W Pythonie, biblioteka "random" korzysta z algorytmu Mersenne Twister, który jest jednym z najczęściej stosowanych generatorów liczb pseudolosowych.

Istnieją inne metody generowania liczb losowych, jak np. generator liczb pseudolosowych z wykorzystaniem kryptografii (CSPRNG). Generator ten jest używany do zastosowań bezpieczeństwa i kryptografii, gdzie ważny jest wysoki poziom losowości.
  
Dla szczegółowej kontroli nad generowanymi liczbami, Python oferuje również moduł "random", który umożliwia dostęp do wielu funkcji, takich jak generowanie z dystrybucji normalnej, wybór losowego elementu z sekwencji i przetasowanie sekwencji.

## Zobacz również:

1. Moduł random Python [link](https://docs.python.org/3/library/random.html)
2. Metoda środkowego kwadratu [link](https://en.wikipedia.org/wiki/Middle-square_method)
3. Algorytm Mersenne Twister [link](https://pl.wikipedia.org/wiki/Mersenne_Twister)
4. Generator liczb pseudolosowych z wykorzystaniem kryptografii (CSPRNG) [link](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)