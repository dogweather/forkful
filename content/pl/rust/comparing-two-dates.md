---
title:                "Porównanie dwóch dat"
html_title:           "Rust: Porównanie dwóch dat"
simple_title:         "Porównanie dwóch dat"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego porównanie dwóch dat może być przydatne w programowaniu? Czasem musimy zidentyfikować, która z dwóch dat jest wcześniejsza lub późniejsza, na przykład w celu wyświetlenia danych w określonej kolejności lub sprawdzenia, czy dany termin minął.

## Jak to zrobić

Porównywanie dat w języku Rust jest bardzo proste. Możemy to zrobić za pomocą funkcji `cmp()` lub `partial_cmp()`. Przykładowy kod może wyglądać następująco:

```rust
use std::cmp::Ordering;

let data1 = "2021-01-01";
let data2 = "2022-01-01";

match data1.cmp(&data2) {
    Ordering::Less => println!("Data1 jest wcześniejsza."),
    Ordering::Greater => println!("Data2 jest wcześniejsza."),
    Ordering::Equal => println!("Obie daty są identyczne."),
}
```

W powyższym przykładzie używamy funkcji `cmp()`, która zwraca wartość typu `Ordering`. Możemy również użyć `partial_cmp()`, która zwraca opcję `Option<Ordering>`, co daje więcej elastyczności w przypadku potencjalnych błędów.

```rust
use std::cmp::Ordering;

let data3 = "2021-01-01";
let data4 = "2022-01-01";

if let Some(result) = data3.partial_cmp(&data4) {
    match result {
        Ordering::Less => println!("Data3 jest wcześniejsza."),
        Ordering::Greater => println!("Data4 jest wcześniejsza."),
        Ordering::Equal => println!("Obie daty są identyczne."),
    }
}
```

W powyższym przykładzie korzystamy z opcji `if let` we wzorcu dopasowania, ponieważ funkcja `partial_cmp()` może zwrócić wartość `None` w przypadku błędu.

## Głębsza analiza

Podczas porównywania dat w języku Rust uwzględnia się nie tylko datę, ale również godzinę i strefę czasową. Dzięki temu możemy uniknąć błędów wynikających z różnic w strefach czasowych. Ponadto, Rust ma wbudowane typy dla dat i czasów, co pozwala na bardziej elastyczne i precyzyjne operacje na nich.

## Zobacz także

- Dokumentacja języka Rust: https://www.rust-lang.org/
- Poradnik na temat porównywania dat w języku Rust: https://dev.to/shotexa/rust-dates-how-to-7e7
- Przewodnik po typach dat w języku Rust: https://www.educative.io/courses/learn-rust-from-scratch/demow-jypejJe