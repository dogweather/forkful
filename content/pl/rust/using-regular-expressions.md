---
title:                "Wykorzystanie wyrażeń regularnych"
date:                  2024-01-19
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"

category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regularne wyrażenia (regex) pozwalają przeszukiwać tekst pod kątem wzorców. Programiści korzystają z nich, by szybko znajdować i manipulować danymi.

## How to:
W Rust używamy crate `regex`. Dołącz crate dodając `regex = "1"` do `Cargo.toml`. Oto przykład:
```Rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"(\w+)@(\w+)\.com").unwrap();
    let email = "kontakt@przyklad.com";

    if re.is_match(email) {
        println!("To jest poprawny adres email!");
    }
}
```
Sample output:
```
To jest poprawny adres email!
```

## Deep Dive
Regularne wyrażenia, zapoczątkowane w latach 50., nabrały znaczenia w pracy z Unixem w latach 70. Alternatywy to metody wbudowane w stringi jak `find()` czy `contains()`, ale nie oferują one takiej elastyczności. Implementacja Rust'a opiera się na silniku automatów skończonych, co gwarantuje wysoką wydajność.

## See Also
- Oficjalna dokumentacja Rust: https://doc.rust-lang.org/regex/regex/index.html
- Poradnik Regex w Rust: https://rust-lang-nursery.github.io/rust-cookbook/text/regex.html
- Interaktywny edytor regex do eksperymentowania: https://regexr.com/
