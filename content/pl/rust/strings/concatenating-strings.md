---
aliases:
- /pl/rust/concatenating-strings/
date: 2024-01-20 17:35:40.191452-07:00
description: "\u0141\u0105czenie string\xF3w to po prostu spos\xF3b, by z dw\xF3ch\
  \ czy wi\u0119cej osobnych tekst\xF3w zrobi\u0107 jeden ci\u0105g\u0142y. Programi\u015B\
  ci robi\u0105 to, gdy potrzebuj\u0105 stworzy\u0107\u2026"
lastmod: 2024-02-18 23:08:49.382166
model: gpt-4-1106-preview
summary: "\u0141\u0105czenie string\xF3w to po prostu spos\xF3b, by z dw\xF3ch czy\
  \ wi\u0119cej osobnych tekst\xF3w zrobi\u0107 jeden ci\u0105g\u0142y. Programi\u015B\
  ci robi\u0105 to, gdy potrzebuj\u0105 stworzy\u0107\u2026"
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Łączenie stringów to po prostu sposób, by z dwóch czy więcej osobnych tekstów zrobić jeden ciągły. Programiści robią to, gdy potrzebują stworzyć wiadomości, złożone dane czy po prostu wyświetlić coś spójnie.

## How to: (Jak to zrobić:)
Rust używa różnych metod do łączenia stringów. Oto kilka przykładów:

```Rust
fn main() {
    // Użycie operatora `+`
    let hello = "Cześć".to_string();
    let world = "świat!";
    let hello_world = hello + " " + world;
    println!("{}", hello_world);  // Wypisze "Cześć świat!"

    // Użycie makra `format!`
    let new_hello_world = format!("{} {}", "Cześć", "świat!");
    println!("{}", new_hello_world);  // Wypisze "Cześć świat!"

    // Użycie metody `push_str` do dołączenia stringa do istniejącego String
    let mut hello = "Cześć".to_string();
    hello.push_str(" świat!");
    println!("{}", hello);  // Wypisze "Cześć świat!"

    // Użycie metody `push` do dodania pojedynczego znaku
    let mut exclamation = String::from("!");
    exclamation.push('!');
    println!("{}", exclamation);  // Wypisze "!!"
}
```

## Deep Dive (Głębsze zanurzenie)
Concatenating strings – to dość nowa historia w Rust, zmieniającą się wraz z ewolucją języka. Początkowo Rust bazował na zasadach podobnych do języka C, ale z czasem wprowadzono nowe metody, takie jak `push_str` i `format!`, ułatwiające życie programistów.

Alternatywy jak `.join()` pozwalają na szybkie łączenie kolekcji stringów z separatorem. Implementacja tych funkcji wykorzystuje cechy alokacji pamięci i może być bardziej wydajna niż ciągłe używanie `+`.

Kluczowym aspektem jest to, jak Rust radzi sobie z pamięcią przy łączeniu stringów. Nie ma tutaj automatycznego zarządzania pamięcią jak w niektórych językach wyższego poziomu. Właśnie dlatego Rust wymaga od deweloperów, by świadomie decydować o alokacji i życiu danych, co przy łączeniu stringów często kończy się zabawą z własnością (`ownership`) i wypożyczaniem (`borrowing`).

## See Also (Zobacz również)
- [The Rust Programming Language – Chapter 8.2: Strings](https://doc.rust-lang.org/book/ch08-02-strings.html) - Oficjalna dokumentacja o stringach w Rust.
- [Rust By Example – 2.5 Strings](https://doc.rust-lang.org/rust-by-example/std/str.html) - Interaktywne przykłady zarządzania stringami.
