---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Zastępowanie tekstu polega na identyfikacji ciągów znaków lub wzorców w tekście i zamianie ich na inne ciągi. Programiści robią to m.in. do czyszczenia danych, transformowania formatów lub naprawiania błędów.

## Jak to zrobić:
Podstawowe wyszukiwanie i zastępowanie w Rust poprzez funkcję `replace` dla `String`. Przykładowy kod wygląda tak:

```Rust
fn main() {
    let tekst = String::from("Cześć, Świecie!");
    let tekst_nowy = tekst.replace("Świecie", "Programisto");
    println!("{}", tekst_nowy);
}
```
Po uruchomieniu kodu, wynik będzie następujący:

```
Cześć, Programisto!
```
## Głębsze zanurzenie
Wyszukiwanie i zastępowanie tekstu w świecie informatyki nie jest nowym konceptem, ale Rust podchodzi do niego w unikalny sposób, dając programistom większą kontrolę nad procesem. Istnieją także alternatywne metody, takie jak korzystanie z wyrażeń regularnych dla bardziej skomplikowanych wzorców.

Co do szczegółów implementacji, funkcja `replace` działa na dane niezmienne i tworzy nowy `String`, zamiast modyfikować istniejący. To oznacza, że stare dane nie są uszkodzone, co może być bardzo ważne w wielu przypadkach.

## Zobacz także
1. Dokumentacja Rust Strings: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
2. Wprowadzenie do wyrażeń regularnych w Rust: https://rust-lang-nursery.github.io/rust-cookbook/text/regex.html
3. Wyszukiwanie i zastępowanie tekstu w Rust za pomocą 'regex': https://stevedonovan.github.io/rustifications/2017/09/09/common-regex-macros.html