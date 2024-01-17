---
title:                "Łączenie ciągów znaków"
html_title:           "Rust: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego?

Łączenie stringów, czyli łączenie tekstowych danych, jest ważnym elementem programowania. Służy do tworzenia większych lub bardziej złożonych ciągów znaków, które mogą być wyświetlone lub przetworzone przez program. Programiści często korzystają z tej techniki, aby ułatwić sobie pracę z tekstowymi danymi.

## Jak to zrobić?

```Rust
let first_name = "John";
let last_name = "Smith";
let full_name = format!("{} {}", first_name, last_name);
println!("Witaj, {}", full_name);
```

Output:
```
Witaj, John Smith
```

## W głębi

Łączenie stringów jest techniką powszechnie stosowaną w programowaniu od lat. Wcześniej programiści często korzystali z funkcji takich jak `strcat` lub operatora `+`. Jednak w języku Rust, używa się funkcji `format!` lub makra `println!` zamiast tych starszych metod. Jest to bezpieczniejsze i bardziej wydajne podejście do łączenia stringów.

## Zobacz też

- Dokumentacja języka Rust do funkcji `format!`: https://doc.rust-lang.org/std/fmt/
- Porównanie różnych metod łączenia stringów w języku Rust: https://doc.rust-lang.org/book/ch08-02-strings.html