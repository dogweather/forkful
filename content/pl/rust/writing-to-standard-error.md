---
title:                "Rust: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego?

Pisanie do standardowego błędu może wydawać się niepotrzebnym lub nawet irytującym zadaniem podczas programowania w Rust. Jednak jest to ważna praktyka, która może pomóc w diagnozowaniu błędów i ułatwić debugowanie programów. W tym artykule dowiesz się, dlaczego warto pisać do standardowego błędu i jak to zrobić w języku Rust.

## Jak to zrobić?

Pisanie do standardowego błędu w języku Rust jest proste i wymaga użycia metody ``eprintln!``. Przykładowy kod wyglądałby następująco:

```Rust
fn main() {
    let name = "Kasia";
    let age = 25;
    eprintln!("Witaj, nazywam się {} i mam {} lat.", name, age);
}
```

Output tego kodu wyglądałby następująco:

```text
Witaj, nazywam się Kasia i mam 25 lat.
```

## Deep Dive

Standardowy błąd jest jednym z trzech strumieni wyjścia w języku Rust, obok standardowego wyjścia i standardowego wyjścia błędów. Różni się on od pozostałych dwóch tym, że jest pisany do konsoli błędów, zamiast do standardowego wyjścia. Dzięki temu, nawet w przypadku wystąpienia błędów, informacje są wyświetlane w odpowiednim miejscu, a program może kontynuować swoje działanie. Oczywiście, pisanie do standardowego błędu nie powinno być wykorzystywane jako główna metoda raportowania błędów w programie, jednak może być bardzo przydatne w procesie debugowania i analizy kodu.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o standardowym wyjściu, standardowym wyjściu błędów i innych aspektach pisania w Rust, polecamy zapoznanie się z poniższymi linkami:

- https://doc.rust-lang.org/std/io/struct.Stderr.html
- https://doc.rust-lang.org/book/ch01-03-hello-cargo.html#inputoutput
- https://doc.rust-lang.org/book/ch08-01-vectors.html