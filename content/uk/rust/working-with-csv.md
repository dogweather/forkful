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

## Чому

Створення, збереження та обробка даних у форматі CSV є поширеною практикою в сучасному світі, особливо для тих, хто працює зі значними обсягами інформації. Використання мови програмування Rust дає змогу ефективно та надійно працювати з CSV-файлами, що робить її привабливим вибором для тих, хто хоче працювати з цим форматом даних.

## Як

Для роботи з CSV-файлами у Rust потрібно додати бібліотеку `csv` до вашого проекту у файлі `Cargo.toml`. Після цього в коді необхідно імпортувати бібліотеку за допомогою `use csv::Reader`. Далі можна використовувати методи цього модуля для читання та запису даних, наприклад:

```Rust
use csv::Reader;

fn main() {
    let mut reader = Reader::from_path("data.csv").unwrap();

    for result in reader.records() {
        let record = result.unwrap();
        println!("Name: {}, Age: {}", record[0], record[1]);
    }
}
```

В цьому прикладі ми створюємо читача `Reader`, якому передаємо шлях до нашого CSV-файлу. Потім ми можемо використовувати метод `records()` для ітерації по рядках файлу та отримання даних у вигляді вектору. В цьому випадку ми друкуємо перший та другий стовпці кожного запису.

## Глибокий занурення

Бібліотека `csv` має багато корисних методів для роботи з даними у форматі CSV. Наприклад, ви можете використовувати метод `has_headers()` для перевірки наявності заголовків у файлі, а `trim()` для видалення пробілів з кінця рядка.

Крім того, за допомогою бібліотеки `csv` можна також записувати дані у CSV-файл за допомогою методу `Writer::from_path()` та методів `write_record()` чи `write_all()`.

## Дивіться також

- Документація бібліотеки `csv`: https://docs.rs/csv/
- Бібліотека `csv` у сховищі додатків Cargo: https://crates.io/crates/csv
- Приклади коду для роботи з CSV-файлами у Rust: https://github.com/frewsxcv/rust-csv-examples