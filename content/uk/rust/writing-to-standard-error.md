---
title:    "Rust: Чернетка до стандартної помилки"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Чому

У програмуванні ми часто використовуємо різні методи виведення інформації, одним з яких є запис до стандартного помилкового потоку. Це особливий вид виведення, який дозволяє нам побачити інформацію про помилки в нашому коді. В цій статті ми розглянемо, як написати до стандартного помилкового потоку в мові програмування Rust.

## Як це зробити

Для початку, нам потрібно вставити рядок коду ```use std::io::Write;``` до нашого файла, щоб використовувати функції для запису до потоку. Далі, ми можемо скористатися методом ```eprintln!()``` для виведення інформації до стандартного помилкового потоку. Наприклад:

```Rust
use std::io::Write;

fn main() {
    let error_message = "Oh no! Something went wrong.";

    eprintln!("Error: {}", error_message);
}

```

Цей код виведе наступне у консолі:

```
Error: Oh no! Something went wrong.
```

## Поглиблене дослідження

Крім методу ```eprintln!()```, ми також можемо скористатися функцією ```io::stderr().write_all()```, яка дозволяє нам виводити будь-яку послідовність байтів до стандартного помилкового потоку. Цей метод може бути корисним, коли ми хочемо вивести більш складну інформацію, наприклад, якщо ми хочемо виводити цілі функції або структури даних. Також, ми можемо використовувати метод ```io::stderr().flush()```, щоб спустошити буфер і переконатися, що наша інформація була записана до потоку.

## Дивіться також

- [Документація Rust про запис до стандартного помилкового потоку](https://doc.rust-lang.org/std/io/fn.stderr.html)
- [Виведення до стандартного виводу у мові Rust](https://www.geeksforgeeks.org/output-in-rust-programming/)
- [Поглиблене вивчення стандартного помилкового потоку у мові Rust](https://medium.com/@ilango100/writing-output-to-standard-output-std-out-and-standard-error-std-err-in-rust-program-d6df0e58a6de)