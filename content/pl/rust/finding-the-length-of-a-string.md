---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Rust: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Czemu powinieneś dowiedzieć się, jak obliczyć długość łańcucha znaków w Rust?

Obliczanie długości łańcucha znaków jest częstym zadaniem przy pracy z tekstami i może być przydatne w różnych projektach programistycznych. W tym artykule dowiesz się, jak można to zrobić w języku Rust z przykładami kodu.

## How To

Najprostszym sposobem na obliczenie długości łańcucha znaków w Rust jest użycie metody `len()` na zmiennej typu `String`. Należy jednak pamiętać, że ta metoda zwraca liczbę znaków, a nie bajtów. Przykładowy kod wygląda następująco:

```Rust
let my_string = String::from("Hello World!");
let string_length = my_string.len();

println!("Długość łańcucha znaków: {}", string_length);
```

Ten kod zwróci wartość 12, ponieważ w zmiennej `my_string` znajduje się łańcuch z 12 znakami.

Jeśli chcesz obliczyć długość łańcucha w bajtach, możesz użyć metody `len()` na tablicy bajtów, uzyskanej z `as_bytes()`:

```Rust
let my_string = String::from("Hello World!");
let bytes = my_string.as_bytes();
let byte_length = bytes.len();

println!("Długość łańcucha w bajtach: {}", byte_length);
```

Ten kod zwróci wartość 11, ponieważ znak `ó` w języku polskim zajmuje 2 bajty.

Jeśli chcesz obliczyć długość łańcucha w słowach, możesz podzielić łańcuch na części za pomocą metody `split_whitespace()` i użyć metody `count()` na uzyskanej kolekcji:

```Rust
let my_string = String::from("Hello World!");
let words: Vec<&str> = my_string.split_whitespace().collect();
let word_count = words.len();

println!("Długość łańcucha w słowach: {}", word_count);
```

Ten kod zwróci wartość 2, ponieważ w zmiennej `my_string` znajdują się tylko dwa słowa.

## Deep Dive

Chcesz dowiedzieć się, jak dokładnie działa metoda `len()`? Zajrzyjmy do dokumentacji Rust:

> Metoda `len()` nałożona jest na zmienne typów `String`, `Vec` oraz `VecDeque` i zwraca liczbę elementów w kolekcji.

Oznacza to, że metoda ta jest zaimplementowana właśnie dla tych typów i dokładnie definiuje, jak ma być obliczana długość kolekcji.

Jednym z powodów, dla których metoda `len()` zwraca liczbę znaków, a nie bajtów, jest fakt, że w języku Rust standardowo używa się znaków Unicode, które mogą zajmować więcej niż 1 bajt. Dzięki temu nie musisz martwić się o to, że indeksowanie łańcuchów będzie trudne czy też dojdziemy do problemów z różnymi zestawami znaków w różnych językach.

## See Also

- [Dokumentacja Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [Samouczek Rust](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [FAQ Rust](https://www.rust-lang.org/faq.html)