---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Kapitalizacja tekstu w Rust przekształca pierwszy znak w ciągu na wielką literę. Programiści robią to dla poprawy czytelności, formatowania danych oraz dostosowania do konwencji stylistycznych.

## How to (Jak to zrobić):
Rust nie ma wbudowanej metody do kapitalizacji, ale można to łatwo zrobić samemu. Dawaj:

```Rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "witaj, świecie!";
    println!("{}", capitalize_first(my_string)); // Wyświetla: "Witaj, świecie!"
}
```

## Deep Dive (Głębsze zagłębienie):
Kapitalizacja ciągów w Rust może być niestandardowa, ponieważ język nie ma wbudowanej funkcjonalności jak np. metoda `toUpperCase()` w JavaScript. Historia tego przeoczenia nie jest dokładnie opisana, ale wynika to z filozofii Rust, która preferuje minimalizm i wydajność.

Jako alternatywę, możesz użyć zewnętrznych crate'ów takich jak `heck`, które oferują różne metody manipulacji stringami, w tym `to_title_case()`.

Implementacja customowej funkcji, jak `capitalize_first`, wymaga zrozumienia iteratorów w Rust. Warto też zauważyć, że `.to_uppercase()` zwraca iterator, więc używamy `collect::<String>()`, aby zamienić go z powrotem na `String`.

## See Also (Zobacz również):
- Dokumentacja Rust na temat iteratorów: https://doc.rust-lang.org/std/iter/
- Crates.io, gdzie znajdziesz `heck` i inne przydatne biblioteki: https://crates.io/
- Rust String method docs dla innych operacji na ciągach znaków: https://doc.rust-lang.org/std/string/struct.String.html