---
title:                "Перевірка наявності директорії"
html_title:           "Go: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що і чому?

Перевірка наявності директорії - це процес виявлення, чи існує певний шлях у файловій системі. Програмісти роблять це, щоб уникнути помилок при спробі відкрити або працювати з неіснуючим пошляху.

## Як це робити:

Наведення коду й результат на прикладі мови програмування Rust:

```Rust
use std::path::Path; 

fn main() {
    let dir = Path::new("/var/tmp"); 
    if dir.is_dir() {
        println!("The directory exists!"); 
    } else {
        println!("The directory does not exist!"); 
    }
}
```

При виконанні цього коду, у випадку наявності папки `/var/tmp`, буде виведено:
```
The directory exists!
```
А якщо папки немає, то ви отримаєте:
```
The directory does not exist!
```

## Занурення у деталі

Перевірка наявності директорії - це загальна задача в комп'ютерному програмуванні. Історично, було багато підходів до її вирішення в різних мовах програмування.

Є інші методи перевірки наявності директорії в Rust, наприклад, за допомогою функції `metadata` з модулю `std::fs`. Але метод `is_dir`, який наведений вище, є найпростішим і найбільш ідіоматичним для Rust.

Щодо деталей реалізації, метод `is_dir` використовує системні виклики для звернення до файлової системи й отримання метаданих про шлях. Процес може відрізнятись в залежності від того, на якій операційній системі запущена програма.

## Дивіться також

1. Документація Rust по `Path`: [https://doc.rust-lang.org/std/path/struct.Path.html](https://doc.rust-lang.org/std/path/struct.Path.html)
2. Курс про основи програмування на Rust: [https://www.rust-lang.org/learn](https://www.rust-lang.org/learn)
3. Вікіпедія про файлову систему: [https://uk.wikipedia.org/wiki/Файлова_система](https://uk.wikipedia.org/wiki/Файлова_система)