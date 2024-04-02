---
date: 2024-01-26 04:10:44.557130-07:00
description: "Rust \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u0443\u0454 \u0440\u0456\
  \u0437\u043D\u0456 \u0434\u0435\u0431\u0430\u0433\u0435\u0440\u0438, \u0430\u043B\
  \u0435 \u0437\u0430\u0433\u0430\u043B\u044C\u043D\u043E\u043F\u0440\u0438\u0439\u043D\
  \u044F\u0442\u0438\u043C\u0438 \u0454 `gdb` \u0434\u043B\u044F GNU/Linux \u0430\u0431\
  \u043E `lldb` \u0434\u043B\u044F macOS. \u0412\u0438 \u0442\u0430\u043A\u043E\u0436\
  \ \u043C\u043E\u0433\u043B\u0438 \u0431 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 `rust-gdb` \u0430\u0431\u043E `rust-\u2026"
lastmod: '2024-03-13T22:44:48.944101-06:00'
model: gpt-4-0125-preview
summary: "Rust \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u0443\u0454 \u0440\u0456\
  \u0437\u043D\u0456 \u0434\u0435\u0431\u0430\u0433\u0435\u0440\u0438, \u0430\u043B\
  \u0435 \u0437\u0430\u0433\u0430\u043B\u044C\u043D\u043E\u043F\u0440\u0438\u0439\u043D\
  \u044F\u0442\u0438\u043C\u0438 \u0454 `gdb` \u0434\u043B\u044F GNU/Linux \u0430\u0431\
  \u043E `lldb` \u0434\u043B\u044F macOS. \u0412\u0438 \u0442\u0430\u043A\u043E\u0436\
  \ \u043C\u043E\u0433\u043B\u0438 \u0431 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 `rust-gdb` \u0430\u0431\u043E `rust-\u2026"
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0434\
  \u0435\u0431\u0430\u0433\u0435\u0440\u0430"
weight: 35
---

## Як це зробити:
Rust підтримує різні дебагери, але загальноприйнятими є `gdb` для GNU/Linux або `lldb` для macOS. Ви також могли б використовувати `rust-gdb` або `rust-lldb`, які є обгортками, що красиво відображають значення Rust. Ось приклад:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Лічильник показує: {}", counter);
    }
}
```

Для дебагінгу скомпілюйте з інформацією для дебагу:

```shell
$ rustc -g counter.rs
```

Потім запустіть у `rust-gdb`:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Лічильник показує: 1
(gdb) print counter
$2 = 1
```

## Поглиблений розгляд
Дебагінг існує ще з *давніх часів* перфокарт, і його еволюція була справжнім подарунком. Rust надає власні інструменти з інтеграціями для GDB та LLDB через системно-орієнтовану природу мови.

Альтернативи для дебагінгу коду Rust включають використання інтегрованих середовищ розробки (IDE) з їх вбудованими дебагерами, які деякі вважають більш інтуїтивно зрозумілими. Популярні серед них - CLion з плагіном Rust або Visual Studio Code з розширенням Rust.

Що стосується реалізації, Rust створює символи для дебагу, які розуміють ці дебагери, що є життєво важливим для кроку через код, встановлення точок зупинки та перевірки змінних без втрати розуму.

## Див. також
- Книга Rust про дебагінг: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Ошибках та дебагінгу за версією Rust By Example: https://doc.rust-lang.org/rust-by-example/error.html
- Rust Language Server (RLS), який підживлює розширення Rust для VS Code: https://github.com/rust-lang/rls
- Дебагінг Rust з Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
