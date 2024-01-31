---
title:                "Створення текстового файлу"
date:                  2024-01-19
simple_title:         "Створення текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?
Запис текстового файлу — це процес збереження текстових даних на диск. Програмісти виконують цю задачу для збереження результатів, конфігурацій чи передачі даних між процесами.

## Як це зробити:
```Rust
use std::fs::File;
use std::io::Write;

fn main() {
    let path = "приклад.txt";
    let data = "Привіт, це пробний текст!\n";

    let mut output = File::create(path).expect("Не вдалось створити файл");
    write!(output, "{}", data).expect("Не вдалось записати дані у файл");
}
```
Після запуску у теці з'явиться файл `приклад.txt` із текстом "Привіт, це пробний текст!".

## Поглиблений занурення
Історично, запис файлів у мові Rust еволюціонував до зручності та надійності з більш строгими гарантіями від помилок. Можливі альтернативи включають системні виклики через `std::os::unix` або бібліотеки третіх сторін як `tokio` для асинхронного вводу/виводу. Деталі реалізації включають обробку помилок і використання буферизованих виводів для підвищення продуктивності.

## Дивись також
- [The Rust Programming Language Book](https://doc.rust-lang.org/book/)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
- [Standard Library API (std::fs::File)](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Standard Library API (std::io::Write)](https://doc.rust-lang.org/std/io/trait.Write.html)
