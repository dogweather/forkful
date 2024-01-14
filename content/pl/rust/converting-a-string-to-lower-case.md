---
title:                "Rust: Konwersja ciągu znaków na małe litery"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja tekstu na małe litery jest częstym problemem, z którym spotykamy się w codziennej pracy programistycznej. Powodem może być np. potrzeba porównania dwóch ciągów znaków bez uwzględnienia wielkości liter. W takich sytuacjach konieczne jest przekształcenie tekstu na małe litery. W tym artykule dowiesz się jak to zrobić w języku Rust.

## Jak to zrobić

W języku Rust istnieją różne metody konwertowania tekstu na małe litery, a każda może być wybrana w zależności od potrzeb. Jedną z najłatwiejszych i najczęściej używanych jest metoda `to_lowercase()`.

```Rust
let text = "HELLO WORLD";
let lower_case = text.to_lowercase();

println!("{}", lower_case);

// Output: hello world
```

Jeśli chcemy zachować oryginalną wartość zmiennej `text`, możemy użyć metody `to_lowercase()` w połączeniu z `borrow()`.

```Rust
let mut text = "HELLO WORLD".to_string();
let lower_case = text.to_lowercase().borrow();

println!("{}", text);

// Output: HELLO WORLD
```

Aby uzyskać dostęp tylko do pierwszej litery w małej formatce, można użyć metody `make_ascii_lowercase()`.

```Rust
let mut text = "Hello world".to_string();
text.make_ascii_lowercase();

println!("{}", text);

// Output: hello world
```

## Vertigo

Proces konwersji tekstu na małe litery w języku Rust można nazwać "vertigo" (od ang. vertiginous - zawrotny). Jest to nawiązanie do struktury danych przechowującej małe litery, która przypomina schody lub spiralę.

Tak naprawdę proces konwersji wygląda trochę bardziej skomplikowanie, ponieważ język Rust nie operuje na samym tekście, a na indeksach i offsetach do niego. Dzięki temu jest w stanie dokonać zmiany na poziomie rekordu, a nie tylko pojedynczego znaku.

## Zobacz także

- [Dokumentacja języka Rust](https://www.rust-lang.org/)
- [Przykładowy projekt w języku Rust](https://github.com/rust-lang/rustlings)
- [Poradnik dla początkujących w języku Rust](https://doc.rust-lang.org/book/)