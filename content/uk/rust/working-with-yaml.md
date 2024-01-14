---
title:                "Rust: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Також, чому
Робота з YAML має багато практичних застосувань у програмуванні на Rust. Вона дозволяє зберігати дані у текстовому форматі, що полегшує комунікацію і обмін даними між різними програмами.

## Як
```Rust
use std::fs::File;
use std::io::Write;

fn main() {
    // Створення нового YAML файлу
    let mut file = File::create("persons.yaml").unwrap();

    // Створення даних для запису у файл
    let persons = vec![
        ("John Smith", 24),
        ("Anna Johnson", 30),
        ("Michael Williams", 42),
    ];

    // Запис даних у YAML формат у файл
    write!(file, "---
persons:
")?;
    for (name, age) in persons {
        write!(file, "    - name: {}
      age: {}
", name, age)?;
    }
}
```
В результаті виконання програми буде створено файл "persons.yaml" з наступним вмістом:
```
---
persons:
    - name: John Smith
      age: 24
    - name: Anna Johnson
      age: 30
    - name: Michael Williams
      age: 42
```

## Глибоке дослідження
При роботі з YAML важливо дотримуватися правильного формату. Часто виникають проблеми зі збереженням спеціальних символів, таких як подвійні лапки або двокрапки. Для уникнення цих проблем, корисно використовувати бібліотеки, які вже враховують ці випадки, наприклад [serde-yaml](https://github.com/dtolnay/serde-yaml).

## Дивіться також
- [Програмування на Rust: початковий посібник](http://uk.rust-lang.org/learn/get-started)
- [Офіційна документація по роботі з YAML у Rust](https://docs.rs/serde_yaml/)
- [Стаття про структури даних у YAML](https://yaml.org/spec/1.2/spec.html#Data%20Structures)