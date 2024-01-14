---
title:                "Rust: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Чому

Створення тимчасового файлу - це важлива функція у багатьох програмах, особливо в тих, які займаються операціями з файлами. Це дає змогу тимчасово зберігати дані або використання певних ресурсів в процесі виконання програми.

# Як це зробити

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // Створення тимчасового файлу
    let mut temp_file = File::create("/tmp/my_temp_file.txt").expect("Не можу створити файл");

    // Записуємо дані у файл
    temp_file.write_all(b"Привіт, це тимчасовий файл!");

    // Очищуємо буфер та зберігаємо файл
    temp_file.flush().expect("Не можу зберегти файл");
}
```

На виході ми отримаємо новий файл з назвою "/tmp/my_temp_file.txt", який можна використовувати у програмі.

# Глибоке поринання

Щоб створити тимчасовий файл у Rust, ми використовуємо функцію File::create() зі стандартної бібліотеки. Вона повертає Result тип, який містить File об'єкт у випадку успішного створення файлу. Ми можемо обробити помилки за допомогою методу expect() або використовувати match для більш детальної обробки.

Крім того, ми можемо використовувати функцію std::env::temp_dir(), яка повертає шлях до системного каталогу для тимчасових файлів, і згенерувати унікальне ім'я для нашого файлу, використовуючи функцію std::fs::TempFile::new().

# Дивіться також

- [Документація з функції File::create()](https://doc.rust-lang.org/std/fs/struct.File.html#method.create)
- [Документація з функції std::env::temp_dir()](https://doc.rust-lang.org/std/env/fn.temp_dir.html)
- [Документація з функції std::fs::TempFile::new()](https://doc.rust-lang.org/std/fs/struct.TempFile.html#method.new)