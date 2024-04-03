---
date: 2024-01-26 00:58:00.680732-07:00
description: "Jak to zrobi\u0107: Rust radzi sobie z b\u0142\u0119dami na dwa g\u0142\
  \xF3wne sposoby: odzyskiwalne i nieodzyskiwalne b\u0142\u0119dy. Przyjrzyjmy si\u0119\
  \ obu. Odzyskiwalne b\u0142\u0119dy u\u017Cywaj\u0105\u2026"
lastmod: '2024-03-13T22:44:35.192643-06:00'
model: gpt-4-1106-preview
summary: "Rust radzi sobie z b\u0142\u0119dami na dwa g\u0142\xF3wne sposoby."
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
weight: 16
---

## Jak to zrobić:
Rust radzi sobie z błędami na dwa główne sposoby: odzyskiwalne i nieodzyskiwalne błędy. Przyjrzyjmy się obu.

Odzyskiwalne błędy używają `Result<T, E>`:

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("Plik otwarty pomyślnie."),
        Err(_e) => println!("Nie udało się otworzyć pliku."),
    }
}
```

Wynik może być albo "Plik otwarty pomyślnie." albo "Nie udało się otworzyć pliku." w zależności od Twojego `hello.txt`.

Dla nieodzyskiwalnych błędów używamy `panic!`:

```Rust
fn main() {
    // To spowoduje panikę programu, bo prawdopodobnie plik nie istnieje.
    let _f = File::open("nowhere.txt").unwrap();
}
```

Uruchom to, a zobaczysz komunikat o panice. Twój program zatrzyma się w miejscu.

## Głębsze spojrzenie
Historycznie, obsługa błędów w programowaniu była bałaganem. Rust robi to dobrze, jasno rozróżniając odzyskiwalne i nieodzyskiwalne błędy.

Enum `Result` jest dla odzyskiwalnych błędów. Jest to jasne – obsługujesz wariant `Ok` lub `Err`. Masz też metody takie jak `unwrap()` czy `expect()`, ale są to szybkie i brudne skróty, które mogą prowadzić do `panic!`.

`panic!` to sposób Rusta na oznajmienie, że stało się coś naprawdę złego i nie potrafi sobie z tym poradzić. To jak nieodzyskiwalny błąd, który natychmiast zatrzymuje wykonanie programu. Panika w Rust jest często odczuwana z błędami, których nie spodziewasz się obsłużyć, na przykład przy indeksowaniu poza granice tablicy.

Obsługa błędów poprzez zwracanie `Result` jest preferowaną metodą, gdy spodziewasz się radzenia sobie z błędami. To idiomatyczny Rust, co oznacza, że to sposób, na który zgodzili się programiści Rust. Jest jeszcze `Option<T>` dla sytuacji, kiedy błąd to po prostu coś jest `None` zamiast `Some(T)`. Chodzi o to, aby oczekiwać niespodziewanego bez strachu.

Alternatywy? Jasne, możesz użyć innych crate'ów do obsługi błędów dla większych funkcji lub ergonomicznego użytku. Na przykład `anyhow` dla prostych błędów, lub `thiserror` dla błędów w kodzie bibliotek.

## Zobacz również
Zainteresowany głębszym zanurzeniem? Oto, gdzie możesz się udać:

- [Rust Book on Error Handling](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - Świetne miejsce do zrozumienia filozofii obsługi błędów w Rust.
- [Rust by Example: Error handling](https://doc.rust-lang.org/rust-by-example/error.html) - Interaktywne przykłady, abyś mógł pobrudzić ręce.

Pamiętaj, dobra obsługa błędów to nie tylko kodowanie; to dbanie o użytkowników Twojego kodu. Szczęśliwego kodowania!
