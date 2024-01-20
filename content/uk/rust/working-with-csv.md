---
title:                "Робота з csv"
html_title:           "Rust: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з CSV (Comma-Separated Values) - це процес читання та запису даних у файл, де дані розділені за допомогою ком знаків. Це дуже популярний формат для обміну даними у програмуванні та аналітиці. Програмісти використовують цей формат, оскільки він зручний та ефективний для роботи з даними.

## Як це зробити:
Давайте подивимося, як легко можна прочитати дані з CSV файлу та записати зміни у нього використовуючи Rust. Для цього ми будемо використовувати бібліотеку csv, яка доступна на crates.io.

```
fn main() {
    use std::error::Error;
    use std::fs::File;
 
    // Читання даних з CSV файлу
    let mut reader = csv::Reader::from_path("users.csv").unwrap();
    for result in reader.records() {
        let record = result.unwrap();
        println!("{:?}", record);
    }
 
    // Запис даних у CSV файл
    let mut writer = csv::Writer::from_path("users.csv").unwrap();
    writer.write_record(&["Ім'я", "Прізвище", "Email"]).unwrap();
    writer
        .write_record(&["Іван", "Іванов", "ivan@gmail.com"])
        .unwrap();
    writer
        .write_record(&["Олександра", "Ковальчук", "alexandra@gmail.com"])
        .unwrap();
    writer.flush().unwrap();
}
```
Виходом програми буде виведення всіх записів з файлу та оновлений файл із доданими новими даними.

## Огляд:
CSV формат має свою історію, його спочатку використовували для передачі даних у табличному форматі між електронними таблицями. Існують інші альтернативи, такі як JSON або XML, проте CSV залишається дуже популярним серед програмістів через його простоту та легку читабельність. У Rust, бібліотека csv є однією з найбільш популярних для роботи з CSV файлами.

## Дивись також:
- [Документація по бібліотеці csv](https://docs.rs/csv/1.0.0/csv/)
- [Історія та розширення формату CSV](https://www.computerhope.com/issues/ch001356.htm)