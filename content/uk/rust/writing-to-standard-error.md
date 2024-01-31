---
title:                "Запис в стандартний потік помилок"
date:                  2024-01-19
html_title:           "Arduino: Запис в стандартний потік помилок"
simple_title:         "Запис в стандартний потік помилок"

category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?

Запис у стандартну помилку - це виведення тексту не в основний потік програми, а в спеціальний, для повідомлень про помилки. Програмісти роблять це, аби відділити звичайні повідомлення від помилок, спростити логування та аналіз проблем.

## Як робити:

```Rust
use std::io::{self, Write};

fn main() {
    writeln!(io::stderr(), "Помилка: Щось пішло не так!").unwrap();
}
```

Вивід буде помилкою в консолі.

## Поглиблений аналіз:

До стандартного потоку помилок (stderr) звертаються з UNIX часів. Альтернативи: логувальники або зовнішні бібліотеки. Rust реалізує stderr через `std::io` модуль, що дозволяє ефективно обробляти помилки.

## Дивіться також:

- [Rust Book std::io](https://doc.rust-lang.org/std/io/)
- [Rust by Example - Stderr](https://doc.rust-lang.org/rust-by-example/std_misc/process/pipe.html)
- [Rust Error Handling](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
