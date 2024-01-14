---
title:                "Rust: Znajdowanie długości ciągu znaków."
simple_title:         "Znajdowanie długości ciągu znaków."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Długość ciągu znaków jest często ważnym elementem w programowaniu. Znajomość długości ciągu może pomóc w działaniach takich jak porównywanie, sortowanie i wyciąganie podciągów. W tym artykule dowiesz się, jak znaleźć długość ciągu w języku Rust.

## Jak to zrobić

Aby uzyskać długość ciągu w języku Rust, możemy skorzystać z metody `len()`, która jest dostępna dla typu `String`. Poniżej znajduje się przykładowy kod wykorzystujący tę metodę:

```Rust
let tekst = String::from("To jest przykładowy ciąg.");
let dlugosc = tekst.len();
println!("Długość ciągu wynosi: {} znaków", dlugosc);
```

Output:

```
Długość ciągu wynosi: 27 znaków
```

Możemy również wykorzystać metodę `chars()` w połączeniu z `count()` aby uzyskać liczbę znaków w ciągu:

```Rust
let tekst = String::from("To jest przykładowy ciąg.");
let dlugosc = tekst.chars().count();
println!("Długość ciągu wynosi: {} znaków", dlugosc);
```

Output:

```
Długość ciągu wynosi: 27 znaków
```

## Głębszy wgląd

W języku Rust długość ciągu jest przechowywana jako liczba bajtów, a nie liczba znaków. Dzieje się tak, ponieważ Rust używa kodowania UTF-8, które może mieć różną długość dla różnych znaków. Dlatego, jeśli chcemy uzyskać liczbę znaków w ciągu, powinniśmy użyć metody `chars()` w połączeniu z `count()`, jak pokazano w sekcji "Jak to zrobić".

## Zobacz też

- [Dokumentacja Rust - String](https://doc.rust-lang.org/std/string/struct.String.html)
- [The Rust Programming Language - Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)

Dziękujemy za przeczytanie tego artykułu. Mamy nadzieję, że teraz wiesz, jak znaleźć długość ciągu w języku Rust. Do zobaczenia w następnym artykule!