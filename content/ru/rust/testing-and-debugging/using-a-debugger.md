---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:19.118976-07:00
description: "\u041A\u0430\u043A: Rust \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u0438\
  \u0432\u0430\u0435\u0442 \u0440\u0430\u0437\u043B\u0438\u0447\u043D\u044B\u0435\
  \ \u043E\u0442\u043B\u0430\u0434\u0447\u0438\u043A\u0438, \u043D\u043E \u043E\u0431\
  \u0449\u0435\u043F\u0440\u0438\u043D\u044F\u0442\u044B\u043C\u0438 \u044F\u0432\u043B\
  \u044F\u044E\u0442\u0441\u044F `gdb` \u0434\u043B\u044F GNU/Linux \u0438\u043B\u0438\
  \ `lldb` \u0434\u043B\u044F macOS. \u0412\u044B \u0442\u0430\u043A\u0436\u0435 \u043C\
  \u043E\u0436\u0435\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C `rust-gdb`\u2026"
lastmod: '2024-03-13T22:44:44.674243-06:00'
model: gpt-4-0125-preview
summary: "Rust \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u0438\u0432\u0430\u0435\u0442\
  \ \u0440\u0430\u0437\u043B\u0438\u0447\u043D\u044B\u0435 \u043E\u0442\u043B\u0430\
  \u0434\u0447\u0438\u043A\u0438, \u043D\u043E \u043E\u0431\u0449\u0435\u043F\u0440\
  \u0438\u043D\u044F\u0442\u044B\u043C\u0438 \u044F\u0432\u043B\u044F\u044E\u0442\u0441\
  \u044F `gdb` \u0434\u043B\u044F GNU/Linux \u0438\u043B\u0438 `lldb` \u0434\u043B\
  \u044F macOS."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u043E\u0442\u043B\u0430\u0434\u0447\u0438\u043A\u0430"
weight: 35
---

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
