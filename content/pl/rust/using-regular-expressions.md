---
title:                "Rust: Używanie wyrażeń regularnych"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions, or "wyrażenia regularne" in Polish, are an essential tool for any programmer looking to manipulate and extract information from strings of text. Whether you're building a parser, validating user input, or performing data analysis, regular expressions can help you efficiently search for and extract patterns within strings. They may seem daunting at first, but with a little practice, you'll find them to be a powerful and indispensable tool in your programming arsenal.

## Jak

Użycie regular expressions w języku Rust jest proste i intuicyjne. Po prostu importuj "regex" moduł i zastosuj metodę "Regex::new" do stworzenia wyrażenia regularnego. Następnie użyj metody "find" do znalezienia dopasowań w danym tekście. Przykładowy kod poniżej pokazuje jak wykorzystać regular expressions do znalezienia numeru telefonu w danym ciągu znaków:

```Rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"[\d]{3}-[\d]{3}-[\d]{3}").unwrap();
    let text = "To jest mój numer telefonu: 123-456-789.";
    let phone_number = re.find(text);

    match phone_number {
        Some(number) => println!("Znaleziony numer telefonu: {}", number.as_str()),
        None => println!("Nie znaleziono numeru telefonu."),
    }
}
```

Po uruchomieniu powyższego kodu, otrzymamy następujący wynik:

```Rust
Znaleziony numer telefonu: 123-456-789
```

Jak widać, używając wyrażeń regularnych, możemy szybko i precyzyjnie znaleźć potrzebne nam informacje w tekście.

## Deep Dive

Jeśli chcesz jeszcze bardziej zgłębić temat wyrażeń regularnych w języku Rust, istnieje wiele zaawansowanych funkcji, takich jak grupowanie, dopasowanie warunkowe czy wyrażenia regularne z załącznikami. Możesz również używać wyrażeń regularnych w celu podzielania lub zamiany tekstu. Aby dowiedzieć się więcej, zapoznaj się z dokumentacją języka Rust dotyczącą wyrażeń regularnych oraz z innych dostępnych źródeł.

## Zobacz również

- Dokumentacja języka Rust dotycząca modułu regex: https://docs.rs/regex/1.4.2/regex/
- Poradnik wyrażeń regularnych w języku Rust: https://rust-lang-nursery.github.io/rust-cookbook/text/regex.html
- Kompletny przewodnik po wyrażeniach regularnych: https://www.regular-expressions.info/