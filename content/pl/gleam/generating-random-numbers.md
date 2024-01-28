---
title:                "Generowanie liczb losowych"
date:                  2024-01-27T20:33:30.598572-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb w programowaniu może być kluczowe do tworzenia symulacji, testowania, kryptografii i gier. W Gleam jest to funkcja, która pozwala deweloperom wprowadzać nieprzewidywalność lub symulować scenariusze z rzeczywistego świata w ich aplikacjach.

## Jak to zrobić:

Aby generować losowe liczby w Gleam, głównie używa się biblioteki `gleam_random`. Biblioteka ta dostarcza funkcje do generowania losowych liczb całkowitych, zmiennoprzecinkowych i więcej. Najpierw upewnij się, że dodałeś `gleam_random` do swojego pliku `rebar.config` lub `mix.exs` jako zależność.

Przejdźmy do kilku przykładów:

### Generowanie losowej liczby całkowitej

Aby wyprodukować losową liczbę całkowitą w określonym zakresie, możesz użyć funkcji `int`:

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

Ta funkcja wygeneruje losową liczbę całkowitą pomiędzy 1 a 10 włącznie.

### Generowanie losowej liczby zmiennoprzecinkowej

Aby uzyskać losową liczbę zmiennoprzecinkową, użyj funkcji `float`. Generuje ona liczbę zmiennoprzecinkową między 0.0 a 1.0:

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### Przykładowy wynik

Uruchomienie tych funkcji może dać wyniki takie jak:

- Dla `generate_random_int()`: `5`
- Dla `generate_random_float()`: `0.84372`

Pamiętaj, że każde wykonanie może prowadzić do różnych wyników ze względu na naturę losowości.

## Pogłębiona analiza

Moduł `gleam_random` implementuje generator liczb pseudolosowych (PRNG), co oznacza, że liczby nie są prawdziwie losowe, ale są trudne do przewidzenia, co naśladuje losowość. PRNG działają, zaczynając od wartości początkowej, znaną jako ziarno, i stosując operacje matematyczne do generowania sekwencji liczb.

Historycznie rzecz biorąc, języki i biblioteki zaimplementowały kilka algorytmów dla PRNG, takich jak Mersenne Twister czy Generator Liniowy Kongruencyjny (LCG). Wybór algorytmu wpływa na jakość „losowości”, przy czym niektóre są bardziej odpowiednie do zastosowań kryptograficznych niż inne. Pomimo tego, że standardowa biblioteka Gleama zapewnia wygodę i łatwość użytkowania z modułem `gleam_random`, może on nie zawsze być najlepszym wyborem do zastosowań wymagających kryptograficznie bezpiecznej losowości. Do celów kryptograficznych deweloperzy powinni szukać bibliotek specjalnie zaprojektowanych do dostarczania kryptograficznie bezpiecznych generatorów liczb pseudolosowych (CSPRNG), które są zaprojektowane by wytrzymać ataki mogące przewidzieć przyszłe liczby na podstawie obserwacji sekwencji wygenerowanych liczb.

Podsumowując, chociaż funkcjonalność generowania liczb losowych w Gleam jest solidna dla ogólnych potrzeb programowania, aplikacje z konkretnymi wymaganiami bezpieczeństwa powinny rozważyć dedykowane rozwiązania kryptograficzne, aby zapewnić integralność i bezpieczeństwo ich generowania liczb losowych.
