---
title:    "Rust: Читання текстового файлу."
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Чому

Програмування мовою Rust - це захоплююча діяльність, яка дозволяє створювати швидкі та ефективні програми. Однією з найважливіших задач у цьому процесі є читання текстових файлів. У цій статті ми розглянемо, як це можна зробити за допомогою мови Rust.

## Як

Для того, щоб прочитати текстовий файл у Rust, ви можете використати стандартну бібліотеку `std::fs`, яка надає функцію `read_to_string`. Ця функція отримує шлях до файлу як аргумент і повертає рядкове значення, що містить весь вміст файлу.

```Rust
use std::fs;

fn main() {
  let file_path = "./sample.txt";
  let file_contents = fs::read_to_string(file_path)
    .expect("Could not read file.");

  println!("File contents: {}", file_contents);
}
```

У цьому прикладі ми використовуємо функцію `read_to_string`, щоб прочитати вміст текстового файлу з шляхом `./sample.txt` і вивести його в консоль.

Ви також можете використати функцію `read` з бібліотеки `std::fs`, яка позволяє читати файл по частинам та обробляти його в більш ефективний спосіб. Наприклад:

```Rust
use std::fs;
use std::io::Read;

fn main() {
  let file_path = "./sample.txt";
  let mut file = fs::File::open(file_path)
    .expect("Could not open file.");
  let mut buffer = String::new();

  file.read_to_string(&mut buffer)
    .expect("Could not read file.");

  println!("File contents: {}", buffer);
}
```

Також існують інші бібліотеки, які дозволяють читати текстові файли у більш розширений спосіб, такі як `serde` або `csv`.

## Deep Dive

Під час читання текстового файлу у Rust, важливо враховувати кодування файлу. Оскільки Rust використовує кодування `UTF-8` за замовчуванням, ви можете стикнутися з проблемами, якщо ваш файл має інше кодування. У такому випадку ви можете використати тип `std::io::BufReader` для зручної обробки бінарних даних.

Також важливо враховувати, що бібліотека `std::fs` використовує платформозалежні системні виклики для читання файлів. Це означає, що ви повинні враховувати різні помилки, які можуть виникнути на різних платформах.

## Дивіться також

- [Документація з читання та запису файлів у Rust] (https://doc.rust-lang.org/std/fs/index.html)
- [Робота з файлами у Rust] (https://dev.to/lgal/working-with-files-in-rust-f47)