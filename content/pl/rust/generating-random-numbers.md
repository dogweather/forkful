---
title:    "Rust: Generowanie losowych liczb"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest ważną częścią wielu programów i aplikacji, szczególnie w kontekście gier, kryptografii i symulacji. Korzystanie z losowych liczb jest niezbędne do stworzenia różnorodności i uczucia losowości w tych systemach. W tym blogu dowiesz się, jak w prosty sposób wygenerować losowe liczby w języku Rust.

## Jak to zrobić

Do generowania losowych liczb w języku Rust możemy skorzystać z biblioteki standardowej rand, która łączy w sobie wiele funkcji związanych z generowaniem liczb pseudolosowych.

```
Rust
use rand::Rng;

fn main() {
    let random_number: u8 = rand::thread_rng().gen_range(1..101);
    println!("Wylosowana liczba to: {}", random_number);
}
```

Powyższy przykład kodu wygeneruje losową liczbę z zakresu od 1 do 100 i wyświetli ją na ekranie. Możemy również użyć różnych innych funkcji z biblioteki rand, takich jak `gen()` czy `gen_bool()`, aby wygenerować odpowiednio liczby w postaci float lub boolean.

## Zanurz się w temat

Generowanie liczb losowych jest trudnym zagadnieniem, ponieważ trudno jest stworzyć dokładnie losową liczbę. W większości przypadków korzysta się z generatorów liczb pseudolosowych, które są oparte na ustalonym algorytmie i ziarnie. Istnieje wiele metod i technik, które mogą poprawić jakość generowanych liczb pseudolosowych, dlatego jeśli chcesz dowiedzieć się więcej, możesz przejrzeć dokumentację biblioteki rand lub poszukać innych materiałów na ten temat.

## Zobacz również

- Dokumentacja biblioteki [rand](https://docs.rs/rand/)
- Przykładowy projekt wykorzystujący generowanie liczb losowych w języku Rust: [Random Number Generator](https://github.com/pchmiel/random-number-generator)
- [Wprowadzenie do generowania liczb pseudolosowych w Rust](https://medium.com/@ckeivan/introduction-to-pseudorandom-number-generation-in-rust-cade908c7549)