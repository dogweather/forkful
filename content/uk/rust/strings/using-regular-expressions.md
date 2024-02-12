---
title:                "Використання регулярних виразів"
aliases: - /uk/rust/using-regular-expressions.md
date:                  2024-02-03T19:18:33.196694-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання регулярних виразів"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

Регулярні вирази, або regex, дозволяють розробникам шукати, зіставляти та маніпулювати рядками за допомогою розширених технік пошуку за шаблоном. У Rust використання regex сприяє ефективному аналізу та обробці текстових даних, роблячи такі завдання, як перевірка даних, пошук та перетворення тексту, більш стройованими та легшими для підтримки.

## Як це зробити:

Бібліотека `regex` у Rust є кращим вибором для роботи з регулярними виразами. Щоб її використати, спочатку вам потрібно додати її до свого `Cargo.toml`:

```toml
[dependencies]
regex = "1"
```

Тоді ви можете почати реалізовувати функціональність regex у вашому коді на Rust. Ось як виконати деякі поширені операції:

### Пошук збігу зі шаблоном у рядку

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-15";

    println!("Чи відповідає текст шаблону дати? {}", re.is_match(date));
    // Вивід: Чи відповідає текст шаблону дати? true
}
```

### Пошук та доступ до збігів

```rust
use regex::Regex;

fn main() {
    let text = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(text) {
        println!("Мова: {}, Рік: {}", &cap[1], &cap[2]);
    }
    // Вивід:
    // Мова: Rust, Рік: 2023
    // Мова: C++, Рік: 2022
    // Мова: Python, Рік: 2021
}
```

### Заміна тексту

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let text = "Rust 2023, C++ 2022, Python 2021";
    let replaced = re.replace_all(text, "$1 було оновлено в $2");

    println!("Оновлений текст: {}", replaced);
    // Вивід: Оновлений текст: Rust було оновлено в 2023, C++ було оновлено в 2022, Python було оновлено в 2021
}
```

### Розбиття тексту за допомогою Regex

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // розбити за будь-яким несловесним символом
    let text = "Rust-C++-Python-Go";

    let fields: Vec<&str> = re.split(text).collect();

    for field in fields {
        println!("Мова: {}", field);
    }
    // Вивід:
    // Мова: Rust
    // Мова: C++
    // Мова: Python
    // Мова: Go
}
```

Ці приклади надають базове керівництво для початку роботи з регулярними виразами у Rust. Коли ваши вимоги стають більш складними, крейт `regex` пропонує багатий функціонал для складного пошуку за шаблонами та завдань маніпуляції текстом.
