---
title:    "Rust: Wyszukiwanie i zastępowanie tekstu"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego

Znajdowanie i zamiana tekstu jest częstym problemem w programowaniu, szczególnie w przypadku dużych projektów. Jest to nie tylko kwestia wygody, ale także może mieć wpływ na wydajność i poprawność działania kodu. Dzięki możliwości wykorzystania Rust do tego celu, możemy mieć pewność, że proces ten będzie szybki i niezawodny.

## Jak to zrobić

Aby przeprowadzić proces znajdowania i zamiany tekstu w Rust, musimy wykorzystać metodę `replace()` na typie `String`. Przykład kodu wygląda następująco:

```Rust
let text = String::from("Witaj, świecie!");
let new_text = text.replace("świecie", "mój drogi");
println!("{}", new_text);
// Output: Witaj, mój drogi!
```

Możemy również wykorzystać metodę `replace()` w połączeniu z metodą `chars()` i `collect()` aby przeprowadzić zmiany na poszczególnych znakach w tekście. Przykład kodu wygląda następująco:

```Rust
let text = String::from("12345");
let new_text = text.chars().collect::<Vec<char>>().iter().map(|c| if *c == '4' { '7' } else { *c }).collect::<String>();
println!("{}", new_text);
// Output: 12375
```

## Głębokie wycieczki

W Rust możemy również wykorzystać bibliotekę regex do przeprowadzania bardziej zaawansowanych operacji na tekście. W przypadku bardziej skomplikowanych wyrażeń regularnych, wykorzystanie tej biblioteki może być bardziej efektywne i wygodne niż ręczne parsowanie tekstu.

Zamiast wykorzystywać prostą metodykę `replace()` możemy również użyć metody `regex.Replace()` do przeprowadzenia zmian w konkretnych częściach tekstu, które pasują do określonego wyrażenia regularnego.

## Zobacz również

- [Dokumentacja Rust dla metody `replace()`](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Przykład użycia biblioteki regex w Rust](https://docs.rs/regex/1.4.2/regex/)
- [Poradnik o wyrażeniach regularnych w Rust](https://cheats.rs/#regex)