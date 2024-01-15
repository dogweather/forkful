---
title:                "Używanie wyrażeń regularnych"
html_title:           "Rust: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek natknąłeś się na potrzebę uzyskania bardzo precyzyjnych wzorców podczas przetwarzania tekstu? Regular expressions w Rust to narzędzie, które może Ci w tym pomóc poprzez wykorzystanie wyrażeń regularnych do dopasowywania i manipulowania tekstem.

## Jak to zrobić?

```Rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\w+@\w+\.\w+$").unwrap(); // Tworzenie wyrażenia regularnego, które dopasowuje poprawny adres email
    let email = "example@example.com";
    
    if re.is_match(email) { // Sprawdzanie, czy podany tekst pasuje do wyrażenia regularnego
        println!("Adres email jest poprawny!");
    } else {
        println!("Nieprawidłowy adres email!");
    }
}
```

Wyrażenia regularne mogą być również wykorzystywane do ekstrakcji konkretnych informacji lub zastąpienia tekstu:

```Rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"https?://(www\.)?(\w+\.)+\w+").unwrap(); // Wyrażenie regularne dopasowujące adresy URL
    let text = "Najlepsze strony do nauki programowania to: https://www.codeacademy.com oraz https://www.udemy.com";
    
    let replaced_text = re.replace_all(text, "<URL>"); // Zastępowanie dopasowanych adresów URL tekstem <URL>
    
    println!("{}", replaced_text); // Output: Najlepsze strony do nauki programowania to: <URL> oraz <URL>
}
```

## Głębszy zanurzenie

Wyrażenia regularne w Rust są oparte na bibliotece `regex` i wykorzystują składnię wyrażeń regularnych znaną z innych języków programowania. Istotne jest również wykorzystanie znaków specjalnych, takich jak `^`, `$`, `+` czy `?`, które pozwalają na precyzyjne dopasowywanie tekstów.

Ponadto, w Rust wyrażenia regularne są kompilowane podczas uruchamiania programu, co pozwala na szybkie i wydajne przetwarzanie tekstu. Istnieje również możliwość wykorzystania wyrażeń regularnych wraz z iteratorami, co pozwala na bardzo wydajną manipulację dużymi zbiorami danych.

## Zobacz również

- [Dokumentacja biblioteki regex w Rust](https://docs.rs/regex/)
- [Poradnik na temat wyrażeń regularnych w Rust](https://turreted.com/posts/rust-regular-expressions/)
- [Przykłady użycia wyrażeń regularnych w programowaniu w Rust](https://medium.com/@CrW468/learning-regexes-through-examples-in-rust-daa05c9a3c29)