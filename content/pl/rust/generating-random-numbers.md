---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego? 
Generowanie losowych liczb to proces tworzenia ciągu liczb, które są nieprzewidywalne i nie powtarzają się. Programiści robią to, aby zapewnić różnorodność danych, symulacje, gry, testy i więcej.

## Jak to zrobić:
Aby wygenerować losową liczbę w Rust, musisz najpierw dodać zależność 'rand' do pliku Cargo.toml:
```Rust 
[dependencies]
rand = "0.8.3"
```
Następnie możesz użyć funkcji `thread_rng().gen_range()` do wygenerowania losowej liczby:
```Rust 
use rand::Rng;

fn main() {
    let num = rand::thread_rng().gen_range(1..101);
    println!("Wylosowana liczba to: {}", num);
}
```
## Głębsze spojrzenie
Początki generowania losowych liczb sięgają starożytności, kiedy ludzie rzucają kostkami do gier. W komputerach używamy algorytmów, takich jak Mersenne Twister albo algorytmu Xorshift, do generowania tych liczb.

W Rust jest wiele alternatyw do 'rand', takich jak 'fastrand' czy 'oorandom'. Każdy z nich ma swoje atuty i wady, które należy rozważyć, zależnie od wymagań projektu. 

Szczegóły implementacyjne 'rand' można znaleźć [tutaj](https://rust-random.github.io/book/guide-rngs.html). 

## Zobacz również
- Dokumentacja 'rand': [https://docs.rs/rand](https://docs.rs/rand)
- Artykuł o generowaniu losowych liczb: [https://en.wikipedia.org/wiki/Random_number_generation](https://en.wikipedia.org/wiki/Random_number_generation)
- Porównanie różnych bibliotek do generowania losowych liczb: [https://www.reddit.com/r/rust/comments/j5bthu/rand_vs_fastrand](https://www.reddit.com/r/rust/comments/j5bthu/rand_vs_fastrand)