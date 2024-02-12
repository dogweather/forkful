---
title:                "Письмо тестів"
aliases:
- /uk/rust/writing-tests/
date:                  2024-02-03T19:32:38.229479-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Написання тестів на Rust включає створення автоматизованих перевірок для забезпечення того, що ваш код працює як очікувалося. Програмісти роблять це, щоб виявити помилки на ранніх етапах, полегшити рефакторинґ та підтримувати якість коду з часом.

## Як це зробити:

Вбудований фреймворк тестування Rust підтримує модульні, інтеграційні та документаційні тести без необхідності в зовнішніх бібліотеках. Тести позначаються за допомогою `#[test]`, і будь-яка функція, позначена так, компілюється як тест.

### Написання модульного тесту:

Розмістіть модульні тести у модулі, який вони тестують, використовуючи підмодуль `tests`, позначений `#[cfg(test)]`, щоб забезпечити їх компіляцію лише під час тестування.

```rust
// lib.rs або main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

Запуск тестів:
```shell
$ cargo test
```

Вивід:
```shell
   Сompiling your_package_name v0.1.0 (/path/to/your_package)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (або src/main.rs)

running 1 test
test tests::it_adds_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### Написання інтеграційних тестів:

Інтеграційні тести розміщуються у каталозі tests на верхньому рівні вашого проекту, поруч із `src`. Кожен файл `.rs` у `tests` компілюється як окрема самостійна крейт.

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### Тестування з популярними бібліотеками сторонніх розробників:

Для більш широких можливостей тестування бібліотека `proptest` може генерувати широкий спектр вхідних даних для тестування функцій.

Додайте `proptest` як залежність для розробки в `Cargo.toml`:

```toml
[dev-dependencies]
proptest = "1.0"
```

Використовуйте `proptest` для запуску одного і того ж тесту з багатьма автоматично генерованими вхідними даними:

```rust
// всередині tests/integration_test.rs або модуля #[cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

Це перевіряє, що `add` не завершується з панікою для широкого діапазону вхідних даних `i32`.
