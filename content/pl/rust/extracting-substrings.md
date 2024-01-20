---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Aby wyodrębnić podłańcuch, rozdzielamy ciąg znaków na mniejsze części. Programiści robią to, aby wyłuskać określone dane z większych bloków tekstu.

## Jak to zrobić:

```Rust
fn main() {
    let slowo = "Programowanie";
    let podlancuch = &slowo[3..6];
    println!("{}", podlancuch);
}
```

Gdy uruchomimy ten kod, wydrukowany zostanie podłańcuch "gram", który jest z przedziału od 3 do 6.

```Rust
fn main() {
    let slowo = "Komputer";
    let poczatek = &slowo[..4];
    println!("{}", poczatek);
}
```

Po uruchomieniu tego kodu, wydrukowane zostanie słowo "Komp", które znajduje się na początku ciągu.

## Głębsze zanurzenie:

Przy wyodrębnianiu podłańucha w Rust, odnosimy się do bajtów, nie indeksów znaków. To dlatego niektóre operacje mogą być trudne do przewidzenia. Jest to konsekwencja wykorzystania przez Rust kodowania UTF-8.

Elegancką alternatywą dla wyżej przedstawionych metod jest użycie funkcji `char_indices()` która zwraca indeksy znaków, nie bajtów.

```Rust
fn main() {
    let kawalek = "Komputer".char_indices().nth(2..4);
    println!("{:?}", kawalek); 
}
```

Jednak ta metoda ma swoje ograniczenia, na przykład nie poradzi sobie z wyodrębnianiem pojedynczych znaków z ciągu.

## Zobacz także:

1. Dokumentacja Rust na temat ciągów znaków: https://doc.rust-lang.org/std/string/index.html
2. Szczegółowy artykuł o kodowaniu UTF-8 w Rust: https://www.ameyalokare.com/rust/2017/10/12/rust-str-vs-String.html
3. Poradnik o manipulacji ciągami znaków w Rust: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html#strings