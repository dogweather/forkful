---
title:                "Написание тестов"
aliases:
- /ru/rust/writing-tests.md
date:                  2024-01-29T00:06:06.701140-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Написание тестов означает создание фрагментов кода, которые проверяют, правильно ли работают другие части кода. Программисты делают это, чтобы заранее выявлять ошибки, обеспечивать функциональность и защищаться от сбоев при внесении новых изменений.

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
