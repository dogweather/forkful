---
title:                "Generowanie losowych liczb"
html_title:           "Rust: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest ważnym elementem w wielu dziedzinach programowania. Jest to przydatne do symulacji, tworzenia gier, generowania danych testowych i wiele innych. Jeśli chcesz poznać, jak w języku Rust wygenerować losowe liczby, ten artykuł jest dla ciebie.

## Jak to zrobić

W Rust istnieje kilka sposobów na generowanie losowych liczb. Możemy skorzystać z biblioteki standardowej lub zewnętrznych bibliotek. Poniżej przedstawimy przykłady dla obu podejść.

### Używając biblioteki standardowej

Do wygenerowania losowej liczby można użyć metody `gen_range()` z modułu `rand`. Najpierw musimy zaimportować moduł, a następnie wywołać tę metodę, podając przedział, z którego chcemy wylosować liczbę.

```rust
use rand::Rng;

let random_number = rand::thread_rng().gen_range(1, 10); // wylosuje liczbę z przedziału [1, 10)
println!("Wylosowana liczba: {}", random_number); // Output: Wylosowana liczba: (losowa liczba z przedziału [1, 10)
```

Jeśli chcemy wylosować wiele liczb w pętli, musimy przechowywać generator w zmiennej, aby uniknąć wywoływania go za każdym razem.

```rust
use rand::Rng;

let mut rng = rand::thread_rng();

for _i in 0..10 {
    let random_number = rng.gen_range(1, 10); // wylosuje liczbę z przedziału [1, 10)
    println!("Wylosowana liczba: {}", random_number); // Output: Wylosowana liczba: (losowa liczba z przedziału [1, 10)
}
```

### Używając zewnętrznej biblioteki

Możemy także użyć zewnętrznej biblioteki do generowania losowych liczb, na przykład `rand_distr`. Ta biblioteka oferuje więcej sposobów na generowanie różnych rodzajów liczb (np. liczby zmiennoprzecinkowe, liczby z rozkładu normalnego). Aby użyć tej biblioteki, po prostu dodaj ją do sekcji `[dependencies]` w pliku `Cargo.toml`.

Poniżej przykład kodu wykorzystującego `rand_distr` do wygenerowania 10 liczb z rozkładu normalnego.

```rust
use rand_distr::{Normal, Distribution};

let normal = Normal::new(10.0, 2.0).unwrap(); // tworzymy rozkład z średnią 10.0 i odchyleniem standardowym 2.0
let mut rng = rand::thread_rng();

for _i in 0..10 {
    let random_number = normal.sample(&mut rng); // pobieramy losową próbkę z rozkładu
    println!("Wylosowana liczba: {}", random_number); // Output: Wylosowana liczba: (losowa liczba z rozkładu normalnego)
}
```

## Deep Dive

Funkcja `gen_range()` używa generatora liczb pseudolosowych, a jego stan jest dzielony globalnie. Z ta metoda istnieje szansa, że wylosowane liczby będą powtarzały się przy wywołaniach w krótkich odstępach czasu. Aby uniknąć tego problemu, można użyć generatora `ThreadRng`, który jest bezpieczny dla wątków i ma swój własny stan.

Jeśli potrzebujemy losowych liczb o większej precyzji niż `f64`, można użyć modułu `num_cpus` w celu wykorzystania wielu wątków do generowania jednej liczby. Jest to wydajniejsze, ale może zwiększyć prawdopodobieństwo powtórzenia liczb.

## See Also

- https://doc.rust-lang.org/rand/rand/index.html - Dokumentacja biblioteki rand
- https://