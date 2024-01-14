---
title:                "Rust: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Ciągłe łączenie stringów jest jedną z podstawowych operacji w programowaniu. Pozwala na tworzenie dłuższych i bardziej skomplikowanych ciągów, co jest szczególnie przydatne w tworzeniu aplikacji internetowych, gier czy nawet skryptów. W języku Rust jest to również ważne ze względu na jego bezpieczeństwo i wydajność.

## Jak to zrobić

Aby połączyć ze sobą dwa stringi w Rust, używamy operatora "+" lub metody "concat()". Poniżej znajdują się przykłady kodu oraz wyników dla obu metod:

```Rust
fn main() {
    let first_string = String::from("Witaj");
    let second_string = String::from("świecie");

    // Użycie operatora "+"
    let combined_string = first_string + " " + &second_string;
    println!("{}", combined_string); // Wyjście: "Witaj świecie"

    // Użycie metody "concat()"
    let combined_string = first_string.concat(" ").concat(&second_string);
    println!("{}", combined_string); // Wyjście: "Witaj świecie"
}
```

W pierwszym przykładzie używamy operatora "+", który umożliwia połączenie tylko dwóch stringów. Dlatego drugi string musi być przekonwertowany na referencję za pomocą operatora "&". Natomiast w drugim przykładzie, wykorzystując metodę "concat()", możemy połączyć dowolną ilość stringów.

## Deep Dive

W języku Rust, przy łączeniu stringów, musimy zadbać o to, aby jeden z nich był własnością (ang. "ownership"), a drugi tylko pożyczką (ang. "borrowing"). W pierwszym przykładzie powyżej, zmienna "first_string" jest własnością i nie musimy jawnie jej przekazywać do metody "concat()". Natomiast zmienna "second_string" jest pożyczką i musimy użyć operatora "&", aby przekazać ją jako referencję.

Ponadto, można również używać makr: "format!" i "push_str()", aby łączyć stringi w Rust. Pierwsze makro służy do formatowania stringów, a drugie metoda dodaje do istniejącego stringa kolejny ciąg znaków.

## Zobacz także

- [Dokumentacja Rust - String](https://doc.rust-lang.org/std/string/index.html)
- [Oficjalna strona języka Rust](https://www.rust-lang.org/)
- [Przewodnik po języku Rust](https://doc.rust-lang.org/book/)