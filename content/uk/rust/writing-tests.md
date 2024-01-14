---
title:                "Rust: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Чому

Написання тестів є важливою частиною процесу розробки програмного забезпечення на Rust. Воно допомагає виявити та усунути помилки ще на ранніх етапах проекту, що зберігає час та ресурси у майбутньому.

## Як написати тести на Rust

Для початку, створимо просту функцію на Rust:

```Rust
fn multiply(a: i32, b: i32) -> i32 {
    return a * b;
}
```

Тепер давайте напишемо тест для перевірки цієї функції:

```Rust
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_multiply() {
        assert_eq!(multiply(2, 3), 6);
        assert_eq!(multiply(-1, 5), -5);
    }
}
```

Перший рядок позначає, що цей файл є модулем для тестування. Далі, ми підключаємо нашу функцію за допомогою ключового слова `use`. У тесті ми використовуємо макрос `assert_eq`, який перевіряє, чи дорівнюють обидва значення. Якщо перевірка не пройде, тест викличе помилку.

## Глибше дослідження

Написання тестів вимагає деякої вправленості та розуміння Rust. Наприклад, користувач може використовувати запитання і тестування, такі як `assert_ne`, `assert!`, або навіть створювати власні макроси для більш специфічних випадків.

Для отримання більш докладної інформації про побудову тестів на Rust, рекомендуємо ознайомитися з офіційною документацією: https://doc.rust-lang.org/book/ch11-00-testing.html

## Дивіться також

- [Документація по тестуванню на Rust] (https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Introducing Rust: Writing Automated Tests](https://www.codementor.io/@stevebennett/introducing-rust-writing-automated-tests-mb3am5ez0)
- [Rust by Example: Testing](https://doc.rust-lang.org/rust-by-example/testing.html)