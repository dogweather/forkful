---
title:                "Rust: Записування рядка з великої літери"
simple_title:         "Записування рядка з великої літери"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Заходячи у програмування, українські читачі можуть стикатися з необхідністю перетворення певних рядків у великі букви. Наприклад, це може стати необхідним для створення заголовків у прикладному програмному інтерфейсі або для форматування користувацьких введених даних. У цьому блозі ми дослідимо, як можна це зробити за допомогою мови програмування Rust.

## Як це зробити

```Rust
fn capitalize_string(string: &str) -> String {
    let capitalized_string = string.to_uppercase();
    return capitalized_string;
}

fn main() {
    let my_string = "hello world";
    let capitalized = capitalize_string(my_string);
    println!("{}", capitalized);
}
```

В цьому кодовому прикладі ми створюємо функцію `capitalize_string`, яка приймає рядок (типу `&str`) як вхідний параметр і повертає великі букви цього рядка у вигляді змінної типу `String`. У функції ми використовуємо метод `to_uppercase()`, який конвертує рядок у великі букви. Потім у функції `main` ми створюємо змінну `my_string` зі значенням `"hello world"` і викликаємо функцію `capitalize_string` для цієї змінної. Нарешті, ми виводимо результат у терміналі.

Результат виконання цього прикладу буде `HELLO WORLD`.

## Глибокий занурення

У мові програмування Rust існує кілька інших методів для роботи з рядками, які можуть допомогти у капіталізації. Наприклад, метод `to_ascii_uppercase()` перетворює рядок у великі букви за допомогою таблиці ASCII, що може бути корисним для міжнародних символів. Також, існує метод `to_lowercase()`, який конвертує рядок у малі букви.

Основні методи для роботи з рядками у мові Rust описані у [документації](https://doc.rust-lang.org/std/primitive.str.html#methods-1).

## Дивись також

- [Rust документація](https://www.rust-lang.org/uk/learn)
- [Стаття про базові концепції мови Rust на Medium](https://medium.com/@sairamkrish/making-sense-of-rusts-syntax-4a7b1cd98e56)
- [Курс програмування на мові Rust від Stanford University](https://web.stanford.edu/class/cs110v/fall2020/)