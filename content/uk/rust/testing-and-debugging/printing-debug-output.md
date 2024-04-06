---
date: 2024-01-20 17:53:22.254910-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F."
lastmod: '2024-04-05T21:53:49.158959-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\u0430\
  \u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\u043D\
  \u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457"
weight: 33
---

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
