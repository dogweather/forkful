---
title:    "Rust: Generowanie losowych liczb"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego?

Generowanie liczb losowych jest ważnym aspektem wielu aplikacji, takich jak gry, symulacje czy kryptografia. W programowaniu w Rust, istnieje wiele sposobów na generowanie liczb losowych, więc jest to ważna umiejętność dla każdego programisty.

## Jak to zrobić?

W Rust można wygenerować liczby losowe za pomocą różnych funkcji, takich jak `rand`, `thread_rng` czy `random`. Poniżej przedstawiamy przykładowy kod, który wykorzystuje najpopularniejszą funkcję - `rand`:

```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let random_number: u32 = rng.gen();
    println!("Wygenerowana liczba losowa: {}", random_number);
}
```

Przypisaliśmy wynik funkcji `gen` do zmiennej `random_number` i wydrukowaliśmy go na konsoli. Oczywiście można również określić zakres generowanych liczb za pomocą funkcji `gen_range` lub dokonać wyboru z listy za pomocą funkcji `choose`.

## Głębsze zagadnienia

Istnieje wiele zalet korzystania z generowania liczb losowych w Rust, takich jak gwarancja odporności na błędy dzięki statycznemu typowaniu czy wydajność dzięki niskopoziomowej implementacji. Jednym z wartościowych modułów jest `rand_core`, który zawiera funkcje do generowania liczb losowych w czystej postaci, bez dodatkowych zależności bibliotek. Warto również zapoznać się z biblioteką `Rand` i jej wykorzystaniem w generowaniu liczb losowych z rozkładem normalnym czy wykorzystaniem ziarna dla zapewnienia powtarzalności wyników.

## Zobacz również

- Dokumentacja biblioteki `rand` [https://docs.rs/rand/0.7.3/rand/](https://docs.rs/rand/0.7.3/rand/)
- Rozbudowane przykłady wykorzystania biblioteki `rand` [https://rust-random.github.io/book/examples.html](https://rust-random.github.io/book/examples.html)
- Szczegółowe informacje o generowaniu liczb losowych w Rust [https://doc.rust-lang.org/std/rand/](https://doc.rust-lang.org/std/rand/)