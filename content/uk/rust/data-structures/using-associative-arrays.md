---
title:                "Використання асоціативних масивів"
aliases:
- /uk/rust/using-associative-arrays.md
date:                  2024-01-30T19:13:12.642660-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання асоціативних масивів"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Асоціативні масиви, або як їх називають ентузіасти Rust - "хеш-мапи", є колекціями, що зберігають дані у парах ключ-значення. Програмісти використовують їх для швидкого пошуку даних, що дозволяє ефективно маніпулювати даними на основі унікальних ключів.

## Як використовувати:

У Rust тип `HashMap` з модулю `std::collections` надає функціональність асоціативних масивів. Ось як ви можете працювати з ними:

```Rust
use std::collections::HashMap;

fn main() {
    // Створення нового HashMap
    let mut scores = HashMap::new();

    // Вставлення значень
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // Доступ до значень
    let team_name = String::from("Blue");
    if let Some(score) = scores.get(&team_name) {
        println!("Бали команди Blue: {}", score); // Вивід: Бали команди Blue: 10
    }

    // Оновлення значення
    scores.entry(String::from("Blue")).and_modify(|e| *e += 5);

    // Ітерація через пари ключ-значення
    for (key, value) in &scores {
        println!("{}: {}", key, value); // Вивід: Blue: 15, Yellow: 50
    }
}
```

## Поглиблений розгляд

`HashMap` у Rust використовує хеш-функцію для відображення ключів на значення, що дозволяє швидко отримувати дані. Проте, ця ефективність має свою ціну: хеш-мапи не зберігають порядок своїх елементів. Це контрастує з іншими реалізаціями асоціативних масивів, такими як у Python (`dict`) або Ruby, які в нових версіях зберігають порядок вставки як особливість. Для випадків, коли порядок пар ключ-значення має значення, розробники Rust можуть розглянути використання `BTreeMap` з модулю `std::collections`, який зберігає порядок, але може пропонувати повільніші вставку та отримання порівняно з `HashMap`. В кінцевому підсумку вибір між `HashMap` і `BTreeMap` залежить від конкретних вимог до порядку та продуктивності.
