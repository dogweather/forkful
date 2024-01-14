---
title:    "Rust: Łączenie ciągów znaków"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu często musimy łączyć ciągi tekstu w jedną dłuższą jednostkę. Jest to szczególnie przydatne przy tworzeniu wyświetlania na ekranie lub przy tworzeniu plików tekstowych. W tym wpisie dowiesz się, dlaczego łączenie ciągów tekstu jest ważną umiejętnością i jak możesz to zrobić w języku Rust.

## Jak to zrobić

Aby połączyć dwa lub więcej ciągów tekstu w języku Rust, możesz użyć metody `concat` lub operatora `+`. Oto przykładowy kod z użyciem obu tych metod:

```Rust
fn main() {
    let string1 = "Cześć";
    let string2 = "połączmy";
    let string3 = "ciągi";
    let result = string1.concat(string2).concat(string3);
    let result2 = string1 + string2 + string3;
    println!("{}", result);
    println!("{}", result2);
}
```

Oczekiwany wynik:

```
Cześćpołączmyciągi
Cześćpołączmyciągi
```

Możesz również używać operatora `+=` do łączenia ciągów tekstu ze zmienną, a nawet stosować formatowanie do łączenia z innymi typami danych, takimi jak liczby.

## Deep Dive

Kiedy używasz operatora `+` do łączenia ciągów tekstu, język Rust automatycznie przekonwertuje je na typ `String` za pomocą metody `to_string()`. Jest to użyteczne, jeśli chcesz łączyć ciągi tekstu z innymi typami danych. Możesz również użyć metody `push_str()` na zmiennej typu `String`, aby dodać ciąg tekstu na samym końcu, zamiast tworzyć nowy obiekt `String`.

## Zobacz też

- Dokumentacja języka Rust: https://doc.rust-lang.org/book/
- Przewodnik dla początkujących w Rust: https://github.com/rust-lang/rustlings
- Funkcje string w języku Rust: https://doc.rust-lang.org/std/primitive.str.html