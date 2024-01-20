---
title:                "Створення тимчасового файлу"
html_title:           "C: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що це і навіщо?

Створення тимчасового файлу - це процес зберігання даних в окремому місці для короткочасного або одноразового використання. Програмісти роблять це, щоб уникнути даних в оперативній пам'яті та проводити операції вводу/виводу.

## Як це зробити:

Спробуємо створити тимчасовий файл і вивести ім'я цього файлу. Сама бібліотека `std::fs::File` забезпечує інструменти для цього.

```Rust
use std::fs::File;
use std::io::Write;
use std::env::temp_dir;

fn main() {
    let mut tmp_path = temp_dir();
    tmp_path.push("temp_file.txt");
    let mut file = File::create(&tmp_path).expect("failed to create file");
    writeln!(file, "Hello, World!").unwrap();

    println!("Saved temporary file at {:?}", tmp_path);
}
```

Коли ви запустите цей код, він створює файл `temp_file.txt` у директорії temp вашої системи і записує в нього рядок "Hello, World!".

## Глибше занурення 

Створення тимчасових файлів бере свій початок з операційних систем UNIX, де навіть процеси запускалися з тимчасових файлів. В альтернативних підходах до створення тимчасових файлів ви можете використовувати бібліотеки, такі як `tempfile` в Rust, які забезпечують вищий рівень абстракції і гарантують автоматичне видалення файлу після його використання. При створенні тимчасового файлу важливо подбати про його видалення після використання, щоб не залишати зайвих даних на диску.

## Дивіться також:

1. [Документація `std::fs::File` в Rust](https://doc.rust-lang.org/std/fs/struct.File.html).
2. [Гайд про працю з файлами в Rust](https://www.tutorialspoint.com/rust/rust_file_io.htm).
3. [Документація `tempfile` бібліотеки в Rust](https://docs.rs/tempfile/3.2.0/tempfile/).