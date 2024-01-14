---
title:                "Rust: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja tekstu na małe litery jest ważnym elementem programowania w języku Rust. Dzięki temu możemy dokonać porównań i operacji na tekście w sposób niezależny od wielkości liter, co jest bardzo przydatne w wielu zastosowaniach. Dlatego warto poznać techniki konwertowania tekstu na małe litery w Rust.

## Jak to zrobić

Konwersja tekstu na małe litery w Rust jest bardzo prosta i wykorzystuje funkcję `to_lowercase()`. Przykładowy kod wyglądałby następująco:

```Rust
let text = "PRZYKŁADOWY TEKST";
println!("{}", text.to_lowercase());
```

Kod ten zwróci nam przekonwertowany tekst, czyli "przykładowy tekst". Funkcja ta działa na typach `String` oraz `&str`, więc możemy ją użyć w zależności od potrzeb. Poniżej przedstawione są dwa przykłady z użyciem obu typów:

```Rust
let string = String::from("PRZYKŁADOWY TEKST");
println!("{}", string.to_lowercase());

let str = "PRZYKŁADOWY TEKST";
println!("{}", str.to_lowercase());
```

Zwrócone zostaną odpowiednio wyniki "przykładowy tekst" dla zmiennej `string` oraz "przykładowy tekst" dla zmiennej `str`.

## Deep Dive

Funkcja `to_lowercase()` wykorzystuje standard Unicode do konwersji tekstu na małe litery. Jest to bardzo ważne w przypadku języków, które używają innych alfabetów niż alfabet łaciński. Ponadto, dzięki wykorzystaniu standardu Unicode, funkcja ta jest odporna na różne kodowania znaków.

W przypadku gdy potrzebujemy dokonać konwersji tylko części tekstu, możemy użyć funkcji `get()` w połączeniu z funkcją `to_lowercase()`. Poniżej przedstawiony jest przykład, w którym konwertowane są tylko pierwsze trzy znaki tekstu:

```Rust
let text = "PRZYKŁADOWY TEKST";
println!("{}S", text.get(0..3).unwrap().to_lowercase());
```

Kod ten zwróci nam wynik "przS", ponieważ tylko pierwsze trzy znaki zostały przekonwertowane na małe litery.

## Zobacz również

- Dokumentacja funkcji `to_lowercase()` w języku Rust: https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase
- Przykładowe programy w języku Rust: https://github.com/rust-lang/rust-by-example
- Kurs programowania w języku Rust: https://www.udemy.com/course/rust-beginner/