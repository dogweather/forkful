---
title:                "Чтение аргументов командной строки"
date:                  2024-01-29T00:00:50.643948-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение аргументов командной строки"

category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/reading-command-line-arguments.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Чтение аргументов командной строки в Rust позволяет программам получать пользовательский ввод при запуске. Это ключ к настройке поведения без графического интерфейса.

## Как это сделать:

Вот самый простой способ захватить аргументы:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
```

Запустите его с помощью `cargo run arg1 arg2`. Вы увидите:

```
["path/to/executable", "arg1", "arg2"]
```

Аккуратный вариант с итераторами:

```Rust
use std::env;

fn main() {
    for arg in env::args().skip(1) {
        println!("{}", arg);
    }
}
```

Теперь попробуйте `cargo run cool stuff`:

```
cool
stuff
```

## Погружение

Исторически, аргументы командной строки являются данью тем временам, когда графические интерфейсы не были широко распространены. Сейчас они отлично подходят для скриптов, серверов или инструментов.

`std::env::args` в Rust использует итератор, что эффективно с точки зрения памяти и лениво. Он также поддерживает Unicode. Есть также `args_os` для необработанных строк ОС.

Для сложного парсинга полезны крейты вроде `clap` или `structopt`. Они разбирают флаги, опции и подкоманды.

## Смотрите также

- [Модуль `std::env` в Rust](https://doc.rust-lang.org/std/env/)
- [Документация крейта `clap`](https://docs.rs/clap/)
- [Книга Rust об аргументах командной строки](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
