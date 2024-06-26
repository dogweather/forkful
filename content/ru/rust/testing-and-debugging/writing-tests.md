---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:06.701140-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0434\u0435\u043B\u0430\u0435\
  \u0442\u0441\u044F: Rust \u0434\u0435\u043B\u0430\u0435\u0442 \u043D\u0430\u043F\
  \u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\u043E\u0432 \u043F\
  \u0440\u043E\u0441\u0442\u044B\u043C. \u0414\u0430\u0432\u0430\u0439\u0442\u0435\
  \ \u043D\u0430\u043F\u0438\u0448\u0435\u043C \u0444\u0443\u043D\u043A\u0446\u0438\
  \u044E \u0438 \u0442\u0435\u0441\u0442 \u0434\u043B\u044F \u043D\u0435\u0451. \u0424\
  \u0443\u043D\u043A\u0446\u0438\u044F."
lastmod: '2024-03-13T22:44:44.672477-06:00'
model: gpt-4-0125-preview
summary: "Rust \u0434\u0435\u043B\u0430\u0435\u0442 \u043D\u0430\u043F\u0438\u0441\
  \u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\u043E\u0432 \u043F\u0440\u043E\
  \u0441\u0442\u044B\u043C."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
weight: 36
---

## Как это делается:
Rust делает написание тестов простым. Давайте напишем функцию и тест для неё.

Функция:

```Rust
fn add_two(a: i32) -> i32 {
    a + 2
}
```

Тест:

```Rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(4, add_two(2));
    }
}
```

Запуск тестов с помощью `cargo test`. Ожидаемый вывод:

```plaintext
   Compiling my_crate v0.1.0 (/path/to/my_crate)
    Finished test [unoptimized + debuginfo] target(s) in 0.31 сек
     Running unittests (target/debug/deps/my_crate-abc123)

running 1 test
test tests::it_adds_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
```

## Подробнее
Исторически тесты пишутся после написания кода (постфактум). Rust поощряет написание тестов одновременно с кодом или даже до его написания (разработка через тестирование, TDD). Существуют и другие формы тестирования - интеграционные тесты, тесты документации и т. д. - каждая из которых имеет свои уникальные детали реализации.

В Rust тесты обычно пишутся в том же файле или в директории `tests/`. Это могут быть модульные тесты (как пример `it_adds_two`), интеграционные тесты (в отдельных файлах) или тесты документации (встроенные в комментарии к документации). Компилятор Rust знает, что функции с `#[test]` должны быть запущены с помощью `cargo test`.

## См. также
- Книга о Rust по теме тестирования: https://doc.rust-lang.org/book/ch11-00-testing.html
- Раздел о тестировании в Rust by Example: https://doc.rust-lang.org/stable/rust-by-example/testing.html
- Рекомендации по тестированию API: https://rust-lang.github.io/api-guidelines/documentation.html#crate-provides-docs-including-rustdoc-and-tests-c-dox
