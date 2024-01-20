---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?

Читання текстового файлу - це процес, коли програма зчитує дані з текстового файлу. Це потрібно для доступу до збережених даних, сторонніх конфігурацій та вхідних потоків.

## Як робимо:

Ось простий приклад, як зчитати текстовий файл у Rust:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::open("foo.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("{}", contents);
  
    Ok(())
}
```

Якщо у файлі "foo.txt" міститься строка "Hello, World!", ви побачите такий вивід:

```rust
Hello, World!
```

## Поглиблений розділ

Час від часу, особливо в головних потоках або серверно-орієнтованих додатках, ви могли б використовувати асинхронне читання файлів. Це особливо корисно, коли вам потрібно читати декілька файлів одночасно. Воно дозволяє вашому додатку залишатися продуктивним без блокування єдиного потоку вводу/виводу. Альтернативно, ви могли б спробувати використовувати стандартні бібліотеки операційних систем, такі як Linux 'fcntl' або Windows 'CreateFile'.

Також, на початку періоду використання Rust можливості взаємодії з файловою системою були обмежені. Це включали примітивної 'File' структуру і кілька її методів. Однак, з часом Rust набув сучасних методів читання файлів.

## Дивись також

- [Читання файлів в асинхронному режимі з 'tokio'](https://docs.rs/tokio/0.2.22/tokio/fs/fn.read_to_string.html)
- [Документація Rust по 'File'](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Стандартні вводи/виводи в Rust](https://doc.rust-lang.org/std/io/index.html)