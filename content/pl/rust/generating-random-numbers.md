---
title:                "Rust: Generowanie losowych liczb"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest ważnym aspektem programowania, który może być wykorzystywany w różnych celach. Może to pomóc w tworzeniu symulacji, losowego wybierania elementów lub w testowaniu programów, które wymagają różnych przypadkowych danych. Dlatego w tym artykule przedstawimy w jaki sposób możemy w łatwy sposób generować liczby losowe w języku Rust.

## Jak to zrobić

Aby wygenerować losową liczbę w Rust, możemy wykorzystać bibliotekę standardową `rand`. Najpierw musimy dodać ją jako zależność w pliku `Cargo.toml`:

```
[dependencies]
rand = "0.8.3"
```

Następnie w pliku `main.rs` możemy zacząć generować liczby losowe:

```
use rand::Rng;

fn main() {
    // wygenerowanie liczby całkowitej z zakresu 1-100
    let random_number = rand::thread_rng().gen_range(1..=100);
    println!("Wylosowana liczba to: {}", random_number);

    // wygenerowanie liczby zmiennoprzecinkowej z zakresu 0-1
    let random_float = rand::thread_rng().gen_range(0.0..1.0);
    println!("Wylosowana liczba to: {}", random_float);
}
```

Po uruchomieniu programu kilka razy, zauważymy, że liczby są generowane w sposób losowy w podanym przez nas zakresie.

## Zanurzenie w temat

Istnieją różne sposoby generowania liczb losowych w Rust, np. wykorzystując bibliotekę `rand` lub `rand_distr`, która pozwala na generowanie liczb według różnych rozkładów prawdopodobieństwa. Warto również pamiętać, że jeśli potrzebujemy wygenerować sekwencję liczb losowych, możemy użyć generatora `SeedableRng` z funkcją `seed_from_u64()` do ustawienia początkowego stanu generatora.

## Zobacz również

- Dokumentacja biblioteki `rand`: https://docs.rs/rand/0.8.3/rand/
- Dokumentacja biblioteki `rand_distr`: https://docs.rs/rand_distr/0.3.2/rand_distr/
- Przykładowe kody wykorzystujące generowanie liczb losowych w Rust: https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=ca81f1016c2e171773a7cc4a33eb9a98