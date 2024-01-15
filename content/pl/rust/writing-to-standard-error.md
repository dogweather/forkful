---
title:                "Pisanie do standardowego wyjścia błędu"
html_title:           "Rust: Pisanie do standardowego wyjścia błędu"
simple_title:         "Pisanie do standardowego wyjścia błędu"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Cześć, witajcie w naszym pierwszym poradniku o programowaniu w Rust. W dzisiejszym artykule poznamy czym jest standard error, dlaczego jest ważny i jak można z niego skorzystać w swoich programach.

## Jak To Zrobić

Pisanie do standard error w Rust jest bardzo proste. Wystarczy użyć funkcji `eprintln!` i podać jako argumenty tekst, który chcemy wypisać oraz ewentualne zmienne, które chcemy wyświetlić. Przykładowy kod wyglądałby następująco:

```Rust
fn main() {
    let name = "Tomasz";
    let age = 32;
    eprintln!("Witaj w programie, {}. Masz już {} lat.", name, age);
}
```

Powyższy kod wypisze nam na standard error wiadomość "Witaj w programie, Tomasz. Masz już 32 lata." Dzięki temu możemy wyświetlać ważne informacje użytkownikom naszych programów, a jednocześnie nie mieszać ich z potencjalnymi wiadomościami błędów, które są wypisywane na standard output.

## Głębsze Zagłębienie

Standard error jest jednym z trzech strumieni wyjścia w języku Rust, pozostałe dwa to standard output i standard input. W przeciwieństwie do standard output, który jest domyślnie przekierowywany na konsolę, standard error jest wykorzystywany głównie do wypisywania błędów i innych ważnych informacji, które użytkownik powinien zobaczyć.

Ponadto, w Rust mamy również dostęp do strumienia stdout_err, który łączy oba strumienie wyjścia, dzięki czemu możemy w jednym miejscu wypisywać zarówno zwykłe komunikaty, jak i błędy.

## Zobacz również

- [Dokumentacja Rust - standard error](https://doc.rust-lang.org/std/io/struct.Stderr.html)
- [Rust by Example - print to standard error](https://doc.rust-lang.org/rust-by-example/hello/print/print_stderr.html)
- [Rust Programming Language Forum](https://users.rust-lang.org/)