---
title:                "Rust: Wydobywanie podciągów"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszym poście porozmawiamy o tym, jak w języku Rust wyciągać podciągi czyli fragmenty ciągów znaków. Jest to przydatna umiejętność, która może przydać się w różnych sytuacjach, na przykład podczas przetwarzania tekstu czy analizowania danych.

## Jak to zrobić

Aby wyciągnąć podciągi w języku Rust, możemy skorzystać z metody `slice()`. W poniższym kodzie, przyjmijmy, że mamy zmienną `text`, która zawiera cały ciąg znaków, a chcemy pobrać tylko jego fragment od 5. do 10. pozycji:

```Rust
let text = "To jest przykładowy tekst";
let substring = &text[5..10]; // tutaj wykorzystujemy metodę slice()
println!("{}", substring); // output: jest 
```

Ważne jest, aby pamiętać o indeksowaniu od zera, dlatego 5. pozycja odpowiada w rzeczywistości 6. znakowi w ciągu. Aby pobrać do końca ciągu, możemy skorzystać z indeksu `..` na końcu naszego zakresu, na przykład `0..` oznacza od początku do końca ciągu.

Również warto wspomnieć, że w języku Rust wyciąganie podciągów jest bardzo wydajne, ponieważ nie tworzy nowych ciągów, a jedynie referencję do istniejącego ciągu, co jest ważne szczególnie w przypadku pracy z dużymi danymi.

## Deep Dive

Metoda `slice()` akceptuje również zmienne typu `String` lub `StringLiteral`. W przypadku typu `String`, możemy użyć metody `as_str()` aby uzyskać ciąg znaków, na którym możemy wywoływać metodę `slice()`.

```Rust
let text = String::from("To jest przykładowy tekst");
let substring = &text[5..10];
println!("{}", substring); // output: jest
```

Jeśli natomiast mamy po prostu ciąg znaków umieszczony w kodzie, możemy wywołać metodę `as_ref()` aby uzyskać referencję do niego i następnie wywołać metodę `slice()`.

```Rust
let text = "To jest przykładowy tekst";
let substring = &text.as_ref()[5..10];
println!("{}", substring); // output: jest
```

W ten sposób możemy wyciągać podciągi z różnych typów danych w języku Rust.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o manipulacji ciągami znaków w języku Rust, zapoznaj się z następującymi materiałami:

- [Oficjalna dokumentacja Rust](https://www.rust-lang.org/learn)
- [Getting started with Rust](https://dev.to/penelope_zone/getting-started-with-rust-1a1e)
- [The Rust Programming Language Book](https://doc.rust-lang.org/book/title-page.html)

Dziękujemy za przeczytanie tego postu i mamy nadzieję, że pomógł on w zrozumieniu jak wyciągać podciągi w języku Rust. Do zobaczenia w kolejnym artykule!