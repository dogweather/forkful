---
title:                "Rust: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest kluczowym aspektem wielu projektów programistycznych, zwłaszcza jeśli chodzi o symulacje, gry czy kryptografię. W języku Rust istnieje wygodny i bezpieczny sposób na generowanie liczb losowych, który pozwala na łatwe wykorzystanie ich w naszych aplikacjach. W tym poście dowiesz się, jakimi mechanizmami dysponuje Rust i jak skorzystać z nich do generowania liczb losowych.

## Jak To Zrobić

Aby wygenerować liczbę losową w Rust, wystarczy użyć funkcji `thread_rng()` z modułu `rand` oraz metody `gen()` zwracanej przez tę funkcję. Poniżej przedstawione jest proste przykładowe użycie:

```
use rand::thread_rng;
use rand::Rng;

fn main() {
    // wygenerowanie jednej liczby losowej z przedziału [0; 10)
    let random_number = thread_rng().gen_range(0, 10);
    println!("Wylosowana liczba: {}", random_number);
    // wygenerowanie wektora, zawierającego 5 losowych liczb z przedziału [0; 100)
    let random_vector: Vec<i32> = thread_rng().sample_iter(&0..100).take(5).collect();
    println!("Wylosowane liczby: {:?}", random_vector);
}
```

Ten kod używa modułu `rand` do wygenerowania liczby losowej z przedziału [0; 10) oraz wektora z pięcioma losowymi liczbami z przedziału [0; 100). Wyniki działania tego kodu będą różne za każdym razem, co potwierdza jego losowy charakter.

## Dogłębna Analiza

W języku Rust dostępnych jest kilka mechanizmów generowania liczb losowych. Jednym z najpopularniejszych jest moduł `rand`, który wykorzystuje generator liczb losowych zaimplementowany w języku C. Można również skorzystać z biblioteki `rand_core`, która zapewnia abstrakcję nad różnymi generatorami.

W przypadku, gdy potrzebujemy bardziej zaawansowanego generatora, np. do celów kryptograficznych, możemy skorzystać z modułu `rand_chacha` lub zaimplementować własny generator w języku Rust. Ważne jest jednak, aby pamiętać o bezpieczeństwie generowanych liczb losowych, szczególnie jeśli chodzi o komunikację z internetem czy bazami danych.

## Zobacz Również

- [Dokumentacja modułu `rand`](https://docs.rs/rand/0.8.5/rand/)
- [Biblioteka `rand_core`](https://crates.io/crates/rand_core)
- [Kryptograficzny generator liczb losowych `rand_chacha`](https://crates.io/crates/rand_chacha)