---
title:                "Написання тестів"
date:                  2024-01-19
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Тести - це код, що перевіряє чи ваш код правильно працює. Ми пишемо тести, аби забезпечити надійність та легше виявити помилки.

## Як робити:
Напишемо простий тест. Припустимо, маємо функцію `add`, яка додає числа. Тест перевіряє, що результат правильний.

```Rust
fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(1, 2), 3);
    }
}
```

Виконайте тести командою `cargo test`. Очікуваний вивід:

```
running 1 test
test tests::test_add ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

## Поглиблений аналіз:
Тестування в Rust базується на модулях з атрибутом `cfg(test)`. Історично Rust запозичив багато концепцій з C++ та Haskell. Крім юніт тестів, є інтеграційні та документаційні. Юніт тести пишуться в тому ж файлі, а інтеграційні - в папці `tests`. 

## Дивіться також:
- [The Rust Programming Language - Testing](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rust by Example - Testing](https://doc.rust-lang.org/rust-by-example/testing.html)
- [Rustaceans - How to write tests](https://rustaceans.org/find-how-to-write-tests)
