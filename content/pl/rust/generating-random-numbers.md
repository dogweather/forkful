---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:49:55.932493-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Generowanie losowych liczb to podstawa wielu aplikacji, od gier po symulacje. Programiści używają tego, aby dodać nieprzewidywalność lub symulować zdarzenia przypadkowe.

## Jak to zrobić:
Użyjemy `rand` crate, żeby zarządzać generowaniem liczb losowych w Rust. Pamiętaj, żeby dodać `rand` do pliku `Cargo.toml`.

```Rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    let liczba: u8 = rng.gen(); // Generuje liczbę losową typu u8
    println!("Wylosowana liczba to: {}", liczba);
}
```

###### Przykładowe Wyjście:
```
Wylosowana liczba to: 157
```

## Głębsze Zanurzenie
Generowanie liczby losowych jest tak stare jak komputery. W latach 40. i 50. używano maszyn ze źródłami szumu do generowania liczb losowych. Alternatywą dla `rand` crate jest używanie API systemu operacyjnego lub własnych algorytmów.

W Rust `rand` crate jest szeroko stosowany z racji jego prostoty i bezpieczeństwa. Używa generatorów typu CSPRNG (Cryptographically Secure Pseudo-Random Number Generator), które są wystarczająco bezpieczne dla kryptografii.

## Zobacz również
- Oficjalna dokumentacja `rand` crate: https://docs.rs/rand
- Rust Cookbook o generowaniu losowych liczb: https://rust-lang-nursery.github.io/rust-cookbook/algorithms/randomness.html
- Dokumentacja Standard Library dla typu `Rng`: https://doc.rust-lang.org/rand/rand/trait.Rng.html