---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Usuwanie znaków pasujących do wzorca to procedura, która usuwa najdłuższy ciąg znaków pasujących do danego wzorca z łańcucha znaków. Programiści robią to przeważnie, aby oczyścić dane, usunąć nadmiarowe znaki lub przygotować łańcuchy do dalszego przetwarzania.

## Jak to zrobić:

Rust udostępnia metodę `replace` na typie `String`, która pozwala na łatwe usuwanie znaków pasujących do wzorca. Oto przykład:

```Rust
let str = String::from("Hello, world!");
let result = str.replace(",", "");
println!("{}", result);
```

Na wyjściu uzyskamy:

```
Hello world!
```

## Głębsze zanurzenie:

Usunięcie znaków pasujących do wzorca jest czynnością wykonywaną od początków informatyki. Alternatywą może być użycie wyrażeń regularnych do bardziej skomplikowanych wzorców. W Rust, jeśli chodzi o implementację, metoda `replace` używa iteratora do przechodzenia przez łańcuch znaków i tworzy nowy łańcuch bez znaków pasujących do wzorca.

## Zobacz także:

1. [Dokumentacja Rust na `String::replace`](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
2. [Poradnik Rust na manipulację łańcuchami](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html) 
3. [Wyrażenia regularne w Rust](https://docs.rs/regex/1.3.9/regex/)