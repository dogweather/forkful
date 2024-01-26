---
title:                "Створення тимчасового файлу"
date:                  2024-01-20T17:41:20.775409-07:00
model:                 gpt-4-1106-preview
simple_title:         "Створення тимчасового файлу"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що і Для Чого?
Тимчасовий файл - це файлик, який ви створюєте для того, щоб тимчасово зберігати дані під час роботи програми. Програмісти використовують їх, щоб працювати з великими даними, тестувати функції або ж кеширувати інформацію, яку не обов’язково зберігати постійно.

## Як це робити:
Rust дозволяє нам створити тимчасовий файл за допомогою бібліотеки `tempfile`. Нижче ви знайдете приклад створення та написання у тимчасовий файл.

```rust
use tempfile::NamedTempFile;
use std::io::{Write, Seek, SeekFrom};

fn main() -> std::io::Result<()> {
    // Створіть новий тимчасовий файл.
    let mut tempfile = NamedTempFile::new()?;

    // Запишіть щось у файл.
    tempfile.write_all(b"Тестовий текст")?;

    // Перейдіть на початок файлу.
    tempfile.seek(SeekFrom::Start(0))?;

    // Файл автоматично видалиться коли `tempfile` покине область видимості.
    Ok(())
}
```

Якщо у вас все вийшло, то програма виконається без помилок, але ви не побачите жодного тексту, тому що файл тимчасовий і буде знищений одразу після закінчення програми.

## Поглиблений Розгляд
Тимчасові файли не новина; вони були в системах UNIX ще з сімдесятих. У Rust, бібліотека `tempfile` використовує унікальні імена для уникнення конфліктів і може створювати файли в стандартній тимчасовій папці системи. Є альтернативи `tempfile`, наприклад, `mkstemp` в С, але `tempfile` забезпечує безпечне API в Rust контексті. Створення тимчасового файлу інкапсулюється в RAII (Resource Acquisition Is Initialization) паттерн в Rust, що означає, що файли будуть автоматично видалені, коли змінна `tempfile` вийде з області видимості, забезпечуючи ефективне управління ресурсами.

## Дивіться Також
- Документація по `tempfile` бібліотеці: [https://docs.rs/tempfile](https://docs.rs/tempfile)
- RAII в Rust: [https://doc.rust-lang.org/book/ch15-03-drop.html](https://doc.rust-lang.org/book/ch15-03-drop.html)
- Rust by Example - TempDir: [https://doc.rust-lang.org/rust-by-example/std_misc/fs.html#tempdir](https://doc.rust-lang.org/rust-by-example/std_misc/fs.html#tempdir)
