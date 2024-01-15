---
title:                "Створення текстового файлу"
html_title:           "Rust: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому
Написання текстового файлу є важливим аспектом програмування в Rust. Використовуючи текстові файли, ви можете зберігати та обробляти великі обсяги інформації, що дозволяє створювати потужні та ефективні програми.

## Як
Нижче наведений приклад коду на Rust, який демонструє створення та запис до текстового файлу:

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // Відкриваємо файл у режимі запису
    let mut file = File::create("myfile.txt").expect("Неможливо створити файл!");

    // Записуємо текст у файл
    file.write_all(b"Це мій перший текстовий файл у Rust!").expect("Неможливо записати до файлу!");

    println!("Файл успішно створено та заповнено інформацією.");
}
```

Вище наведений код використовує модуль `std::fs` для роботи з файлами та метод `write_all()` для запису даних до файлу. Також, використовується конструкція `expect()` для обробки можливих помилок.

Після запуску програми, новий текстовий файл з назвою `myfile.txt` буде створений у тому ж каталозі, де знаходиться програмний файл. За допомогою будь-якого текстового редактора, ви можете перевірити, чи була інформація успішно записана до файлу.

## Deep Dive
У Rust, для роботи з текстовими файлами також можна використовувати методи `read_to_string()` та `read_line()`. Перший метод дозволяє читати весь вміст файлу та повертає його як рядок даних, тоді як другий читає файл по рядках та повертає їх по одному. 

Наприклад:

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // Відкриваємо файл для читання
    let mut file = File::open("myfile.txt").expect("Неможливо відкрити файл!");

    let contents = file.read_to_string().expect("Неможливо прочитати файл!");
    println!("Зміст файлу: {:?}", contents);

    // Читаємо файл по рядках
    let mut file = File::open("myfile.txt").expect("Неможливо відкрити файл!");
    let mut line = String::new();
    loop {
        match file.read_line(&mut line) {
            Ok(0) => break, // Досягнуто кінця файлу
            Ok(_) => { // Рядок прочитано успішно
                println!("Рядок: {}", line);
                line.clear();
            }
            Err(error) => panic!("Помилка: {}", error), // Обробка помилки
        }
    }
}
```

## See Also
- [ofіційна документація Rust](https://www.rust-lang.org/uk)
- [розділ про роботу з файлами](https://doc.rust-lang.org/book/ch12-00-an-io-project.html) в книзі "The Rust Programming Language"
- [приклади коду](https://github.com/rust-lang/rust-by-example/tree/master/src)