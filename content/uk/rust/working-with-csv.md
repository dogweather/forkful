---
title:                "Rust: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Створення програм, що працюють з CSV-файлами, може бути важливою задачею для багатьох програмістів, оскільки цей тип даних широко використовується для збереження табличної інформації, такої як дані про клієнтів, фінансові записи тощо. Робота з CSV дозволяє легко зберігати та обробляти ці дані, що є необхідними для багатьох програм.

## Як

Для початку, потрібно встановити бібліотеку `csv` для мови програмування Rust. У наступних прикладах ми будемо використовувати цю бібліотеку для роботи з CSV-файлами.

```Rust
extern crate csv;

fn main() {
    // Створюємо CSV-файл для запису
    let file = std::fs::File::create("results.csv").expect("Unable to create file");

    // Створюємо записувач для CSV-файлу
    let mut wtr = csv::Writer::from_writer(file);

    // Додаємо дані до нашого CSV-файлу
    wtr.write_record(&["Ім'я", "Прізвище", "Вік"]).expect("Unable to write record");
    wtr.write_record(&["Василь", "Петренко", "35"]).expect("Unable to write record");
    wtr.write_record(&["Наталія", "Коваленко", "28"]).expect("Unable to write record");

    // Очищуємо буфер та зберігаємо дані в файл
    wtr.flush().expect("Unable to flush writer");
}
```

Після запуску цього коду, в нашій папці з'явиться файл `results.csv` з наступним вмістом:

Ім'я | Прізвище | Вік
--- | --- | ---
Василь | Петренко | 35
Наталія | Коваленко | 28

Надалі, ми можемо читати дані з цього CSV-файлу за допомогою наступного коду:

```Rust
// Відкриваємо CSV-файл для читання
let file = std::fs::File::open("results.csv").expect("Unable to open file");

// Створюємо читач для CSV-файлу
let mut rdr = csv::Reader::from_reader(file);

// Ітеруємося по рядках CSV-файлу та виводимо їх на екран
for result in rdr.records() {
    let record = result.expect("Unable to read record");
    println!("{:?}", record);
}

// Виведе в консоль наступне:
// Ok(["Ім'я", "Прізвище", "Вік"])
// Ok(["Василь", "Петренко", "35"])
// Ok(["Наталія", "Коваленко", "28"])
```

## Deep Dive

Бібліотека `csv` для Rust пропонує ряд функціональностей для роботи з CSV-файлами, таких як специфікації формату роздільників і заголовки стовпців. Детальніше про можливості цієї бібліотеки можна дізнат