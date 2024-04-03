---
date: 2024-01-27 20:35:27.329547-07:00
description: "Generowanie losowych liczb w Rust polega na korzystaniu z bibliotek\
  \ do produkcji nieprzewidywalnych warto\u015Bci liczbowych, co jest niezb\u0119\
  dne do zada\u0144 z\u2026"
lastmod: '2024-03-13T22:44:35.180228-06:00'
model: gpt-4-0125-preview
summary: "Generowanie losowych liczb w Rust polega na korzystaniu z bibliotek do produkcji\
  \ nieprzewidywalnych warto\u015Bci liczbowych, co jest niezb\u0119dne do zada\u0144\
  \ z zakresu kryptografii i symulacji, do gier i algorytm\xF3w losowych."
title: Generowanie liczb losowych
weight: 12
---

## Jak to zrobić:
Rust polega na zewnętrznych crate'ach do generowania losowych liczb, przy czym najczęściej używanym jest `rand`. Aby zacząć generować losowe liczby, musisz najpierw dodać `rand` do swojego pliku `Cargo.toml`:

```toml
[dependencies]
rand = "0.8.5"
```

Następnie, można generować losowe liczby używając `rand` w kodzie Rust. Oto przykład generowania losowej liczby całkowitej i liczby zmiennoprzecinkowej:

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    
    // Generowanie losowej liczby całkowitej między 1 a 10
    let random_int: i32 = rng.gen_range(1..=10);
    println!("Losowa liczba całkowita: {}", random_int);
    
    // Generowanie losowej liczby zmiennoprzecinkowej między 0.0 a 1.0
    let random_float: f64 = rng.gen::<f64>();
    println!("Losowa liczba zmiennoprzecinkowa: {}", random_float);
}
```

Przykładowe wyjście może wyglądać tak:

```plaintext
Losowa liczba całkowita: 7
Losowa liczba zmiennoprzecinkowa: 0.9401077112175732
```

Należy zauważyć, że ponowne uruchomienie programu wyprodukuje inne wartości.

## Pogłębiona analiza
Generowanie losowych liczb w Rust, ułatwiane przez `rand` i jego zależności takie jak `getrandom`, stanowi szeroką abstrakcję nad funkcjami systemu operacyjnego i generatorami algorytmicznymi. Historycznie, losowość w informatyce ewoluowała od prostych, przewidywalnych algorytmów do skomplikowanych, kryptograficznie bezpiecznych metod. Podejście Rusta ujęte jest przez jego „pluggable” cechę `Rng`, która może być wspierana przez różne generatory zgodnie z wymaganą jakością losowości i wydajnością.

Dla większości aplikacji, poleganie na `rand` i RNG systemu zapewnia dobrą równowagę między prostotą a entropią. Jednakże, dla aplikacji kryptograficznych, crate'y takie jak `rand` przekazują generowanie ziaren do `getrandom`, który z kolei polega na mechanizmach specyficznych dla systemu operacyjnego (np. `/dev/urandom` w systemach podobnych do Unix), zapewniając kryptograficznie bezpieczną losowość. 

Alternatywnie, jeśli masz specyficzne potrzeby niezaspokojone przez `rand`, rozważenie innych crate'ów lub implementacja własnych generatorów opartych na modelach matematycznych może być drogą do rozwiązania. Niemniej jednak, dla ogromnej większości przypadków użycia, `rand` i jego ekosystem zapewniają solidne rozwiązania, które są zarówno wydajne, jak i proste do zintegrowania z aplikacjami Rust.
