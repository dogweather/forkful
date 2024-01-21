---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:49:43.366651-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Generowanie liczb losowych to proces tworzenia sekwencji liczb, które nie są przewidywalne. Programiści robią to, by symulować zdarzenia losowe, do testów, gier, bezpieczeństwa danych oraz wszędzie tam, gdzie wymagana jest losowość.

## Jak to zrobić:
```Python
import random

# Generowanie pojedynczej liczby losowej z zakresu od 1 do 10
random_number = random.randint(1, 10)
print(random_number)  # np. wydrukuje: 7

# Losowanie elementu z listy
items = ['jabłko', 'banan', 'czereśnia']
chosen_item = random.choice(items)
print(chosen_item)  # np. wydrukuje: banan

# Losowanie z shufflem
numbers = [1, 2, 3, 4, 5]
random.shuffle(numbers)
print(numbers)  # np. wydrukuje: [3, 5, 1, 2, 4]
```

## Deep Dive
Generowanie liczb losowych rozpoczęło swoje życie w matematyce i statystyce. Z czasem zyskało na znaczeniu w informatyce. W Pythonie moduł `random` to standardowy sposób na losowanie, ale nie jest on bezpieczny kryptograficznie. Alternatywą jest moduł `secrets`, który jest bardziej odpowiedni dla bezpieczeństwa danych. Losowanie w Pythonie jest realizowane za pomocą generatora pseudolosowego opartego na algorytmie Mersenne Twister, ale pamiętajmy – nie jest to prawdziwa losowość.

## Zobacz także
- Oficjalna dokumentacja Pythona o modułach `random` i `secrets`: https://docs.python.org/3/library/random.html, https://docs.python.org/3/library/secrets.html
- Artykuł na temat generatorów liczb pseudolosowych: https://pl.wikipedia.org/wiki/Generator_liczb_pseudolosowych
- Informacje o algorytmie Mersenne Twister: https://pl.wikipedia.org/wiki/Mersenne_Twister