---
title:                "Виведення налагоджувальної інформації"
aliases:
- /uk/rust/printing-debug-output/
date:                  2024-01-20T17:53:22.254910-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Виведення діагностичної інформації (debug output) - це процес виведення даних для аналізу та виправлення помилок у коді. Програмісти використовують його, щоб розуміти, що відбувається в програмі під час її виконання та швидко ідентифікувати проблеми.

## Як це зробити:
```Rust
fn main() {
    let number = 42;
    println!("Display: {}", number);
    dbg!(number);
}
```
Виведення:
```
Display: 42
[src/main.rs:4] number = 42
```

## Занурення у деталі:
Виведення для налагодження з'явилося в Rust з самого початку. Макрос `println!` допомагає виводити інформацію в термінал, а `dbg!` виводить не тільки значення змінної, але й інформацію про файл та номер рядка, що є корисним для розуміння контексту. Існують альтернативи стандартному виведенню, як-от: бібліотеки для логування або інтерактивні дебагери, але `println!` та `dbg!` використовуються через свою простоту. За допомогою `dbg!`, Rust бере передане значення, повертає його з макроса (дозволяючи вставляти `dbg!` у різні місця виразу), а також автоматично виводить основну інформацію без потреби у додатковому форматуванні.

## Дивіться також:
- [Rust Book on Debugging](https://doc.rust-lang.org/book/ch09-00-error-handling.html#unrecoverable-errors-with-panic)
- [Rust `std::fmt` formatting guide](https://doc.rust-lang.org/std/fmt/)
- [Rust by Example on Debug trait](https://doc.rust-lang.org/rust-by-example/hello/print/print_debug.html)
- [The Rust `dbg` macro documentation](https://doc.rust-lang.org/std/macro.dbg.html)
