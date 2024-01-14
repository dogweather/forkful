---
title:    "Rust: Znajdowanie długości łańcucha znaków"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego

Znajdowanie długości łańcucha jest podstawową umiejętnością każdego programisty. Czasami chcemy wiedzieć, ile znaków znajduje się w danym łańcuchu, aby móc dokonać odpowiednich obliczeń lub manipulacji na tekście. W tym wpisie dowiesz się, jak możesz szybko i łatwo znaleźć długość łańcucha w języku Rust.

## Jak to zrobić

Aby znaleźć długość łańcucha w Rust, możemy skorzystać z metody `len()`, która jest wbudowana w typ `String`. Przyjmuje ona jako argument odwołanie do łańcucha i zwraca liczbę całkowitą reprezentującą długość łańcucha. Przykładowy kod poniżej pokazuje, jak użyć metody `len()`:

```Rust
let text = "Cześć, świecie!";
let length = text.len(); // przypisanie długości do zmiennej
println!("Długość tekstu to: {}", length); // wypisanie długości na konsolę
```
Output:
```
Długość tekstu to: 14
```

W powyższym przykładzie najpierw tworzymy zmienną `text`, która przechowuje łańcuch "Cześć, świecie!". Następnie, używając metody `len()`, przypisujemy długość tego łańcucha do zmiennej `length`. W ostatniej linijce wypisujemy wynik na konsolę za pomocą funkcji `println!()`.

## Deep Dive

Istnieją również inne metody, które możemy użyć do znalezienia długości łańcucha w Rust. Jedną z nich jest metoda `chars()`, która zwraca iterator po wszystkich znakach w stringu. Dzięki temu możemy łatwo zliczyć liczbę znaków w łańcuchu.

```Rust
let text = "Witaj!";
let length = text.chars().count(); // zliczenie liczby znaków przy użyciu metody count()
println!("Długość tekstu to: {}", length);
```

Output:
```
Długość tekstu to: 6
```

Inną ciekawą metodą jest `bytes()`, która zwraca iterator po bajtach w stringu. Jeśli chcemy zliczyć liczbę bajtów w łańcuchu, możemy skorzystać z metody `len()` na iteratorze.

```Rust
let text = "Rust";
let length = text.bytes().len(); // zliczenie liczby bajtów przy użyciu metody len()
println!("Długość tekstu to: {}", length);
```

Output:
```
Długość tekstu to: 4
```

Warto również zwrócić uwagę, że w języku Rust możemy użyć metody `capacity()` na typie `String`, aby sprawdzić, jak dużo miejsca w pamięci jest zarezerwowane dla danego łańcucha. Może to być przydatne, gdy chcemy zoptymalizować wykorzystanie pamięci w naszych programach.

## Zobacz także

- [Dokumentacja Rust - String](https://doc.rust-lang.org/std/string/struct.String.html)
- [Blog Rust - Dzielenie stringów](https://blog.rust-lang.org/2015/04/24/string-cheat-sheet.html)
- [Rust By Example - String](https://doc.rust-lang.org/rust-by-example/std/str.html)