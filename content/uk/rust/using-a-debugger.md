---
title:                "Використання дебагера"
aliases:
- uk/rust/using-a-debugger.md
date:                  2024-01-26T04:10:44.557130-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання дебагера"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/using-a-debugger.md"
---

{{< edit_this_page >}}

## Що і чому?

Використання дебагера - це ніби наділити себе рентгенівським зором для заглядання у виконання вашого коду. Програмісти роблять це, щоб виявити помилки, зрозуміти потік програми та переконатися, що їхній код чистий як свисток. Це ніби мати друга, який точно вказує, де ви спіткнулися.

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
