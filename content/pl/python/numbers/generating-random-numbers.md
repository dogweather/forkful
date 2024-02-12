---
title:                "Generowanie liczb losowych"
aliases:
- /pl/python/generating-random-numbers/
date:                  2024-01-27T20:35:29.409198-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generowanie liczb losowych"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb polega na tworzeniu liczb, których nie można rozsądnie przewidzieć lepiej niż przez przypadek, co jest kluczowe dla rozwoju symulacji, gier i algorytmów bezpieczeństwa. Programiści robią to, aby wprowadzić nieprzewidywalność lub symulować zjawiska rzeczywiste w swoich aplikacjach.

## Jak to zrobić:

Python dostarcza moduł `random`, który pomaga w generowaniu losowych liczb do różnych zastosowań. Oto jak zacząć:

1. **Importowanie modułu**
    ```Python
    import random
    ```

2. **Generowanie losowej liczby całkowitej**
    Między dowolnymi dwiema liczbami.
    ```Python
    random_integer = random.randint(1, 10)
    print(random_integer)
    ```
    Przykładowy wynik: `7`

3. **Generowanie liczby zmiennoprzecinkowej**
    Między 0 a 1.
    ```Python
    random_float = random.random()
    print(random_float)
    ```
    Przykładowy wynik: `0.436432634653`

    Jeśli potrzebujesz liczby zmiennoprzecinkowej w innym zakresie, pomnóż:
    ```Python
    random_float_range = random.random() * 5  # od 0 do 5
    print(random_float_range)
    ```
    Przykładowy wynik: `3.182093745`

4. **Wybieranie losowego elementu z listy**
    ```Python
    greetings = ['Hello', 'Hi', 'Hey', 'Hola', 'Bonjour']
    print(random.choice(greetings))
    ```
    Przykładowy wynik: `Hola`

5. **Mieszanie listy**
    Idealne dla gier karcianych lub każdej aplikacji wymagającej losowego porządku.
    ```Python
    numbers = list(range(10))
    random.shuffle(numbers)
    print(numbers)
    ```
    Przykładowy wynik: `[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## Dogłębna analiza

Moduł `random` w Pythonie korzysta z generatora liczb pseudolosowych (PRNG), konkretnie z algorytmu Mersenne Twister, który jest dobry do zastosowań ogólnego użytku, ale nie nadaje się do celów kryptograficznych ze względu na jego przewidywalność, jeśli obserwuje się wystarczająco dużo wyników. Moduł `secrets`, wprowadzony w Pythonie 3.6, oferuje lepszą alternatywę do generowania kryptograficznie mocnych liczb losowych, szczególnie przydatną w aplikacjach wrażliwych na bezpieczeństwo. Na przykład generowanie bezpiecznego, losowego tokenu do linku resetowania hasła:

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

Historycznie generowanie naprawdę losowych liczb było wyzwaniem w informatyce, gdzie wczesne metody polegały na zjawiskach fizycznych lub ręcznie wprowadzanych ziarnach. Rozwój i przyjęcie algorytmów takich jak Mersenne Twister (używany domyślnie w module `random` Pythona przynajmniej do mojej ostatniej aktualizacji wiedzy w 2023) oznaczało znaczący postęp. Jednak ciągłe poszukiwanie bardziej bezpiecznych i wydajnych algorytmów doprowadziło do wprowadzenia modułu `secrets` do zadań związanych z kryptografią. Ta ewolucja odzwierciedla rosnące znaczenie bezpieczeństwa w rozwoju oprogramowania i potrzebę bardziej solidnej losowości w aplikacjach, od szyfrowania po generowanie bezpiecznych tokenów.
