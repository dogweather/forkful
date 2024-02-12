---
title:                "Использование отладчика"
aliases: - /ru/rust/using-a-debugger.md
date:                  2024-01-29T00:03:19.118976-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование отладчика"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/using-a-debugger.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Использование отладчика - это как получить рентгеновское зрение, чтобы заглянуть в исполнение вашего кода. Программисты делают это, чтобы обнаружить ошибки, понять поток выполнения программы и убедиться, что их код чист как свисток. Это как иметь друга, который указывает вам точно, где вы споткнулись.

## Как:

Rust поддерживает различные отладчики, но общепринятыми являются `gdb` для GNU/Linux или `lldb` для macOS. Вы также можете использовать `rust-gdb` или `rust-lldb`, которые являются обёртками, красиво отображающими значения Rust. Вот небольшой пример:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Счетчик находится на отметке: {}", counter);
    }
}
```

Чтобы отладить это, скомпилируйте с информацией для отладки:
```shell
$ rustc -g counter.rs
```

Затем запустите его в `rust-gdb`:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Счетчик находится на отметке: 1
(gdb) print counter
$2 = 1
```

## Подробный Разбор

Отладка существует со времён *древних времён* перфокарт, и её эволюция была подарком богов. Rust предоставляет собственные инструменты с интеграцией для GDB и LLDB из-за системно-ориентированной природы языка.

Альтернативы для отладки кода на Rust включают использование интегрированных сред разработки (IDE) со встроенными отладчиками, которые некоторым кажутся более интуитивно понятными. Популярными являются CLion с плагином Rust или Visual Studio Code с расширением для Rust.

Что касается реализации, Rust генерирует символы отладки, которые эти отладчики понимают, что жизненно важно для пошагового прохождения кода, установки точек останова и проверки переменных, не теряя рассудка.

## Смотрите Также

- Книга о Rust по отладке: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Мнение Rust By Example об Ошибках и Отладке: https://doc.rust-lang.org/rust-by-example/error.html
- Сервер Языка Rust (RLS), который управляет расширением Rust для VS Code: https://github.com/rust-lang/rls
- Отладка Rust с помощью Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
