---
title:                "Использование регулярных выражений"
date:                  2024-01-29T00:03:44.263940-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Регулярные выражения, или сокращённо regex, это последовательности символов, формирующих шаблоны поиска. Программисты используют regex для поиска, редактирования или манипуляции текстом путём сопоставления с сложными шаблонами, часто для целей валидации или парсинга.

## Как это сделать:
Rust использует крейт `regex` для операций с регулярными выражениями. Сначала добавьте его в ваш `Cargo.toml`:

```toml
[dependencies]
regex = "1"
```

Затем вы можете сопоставлять строки следующим образом:

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-01";

    println!("Соответствует ли текст шаблону даты? {}", re.is_match(date));
}
```

Вывод:

```
Соответствует ли текст шаблону даты? true
```

Для захвата групп:

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"(\w+)@(\w+)\.(\w+)").unwrap();
    let email = "user@example.com";

    match re.captures(email) {
        Some(caps) => {
            println!("Пользователь: {}, Домен: {}, Расширение: {}", &caps[1], &caps[2], &caps[3]);
        }
        None => println!("Совпадений не найдено."),
    }
}
```

Вывод:

```
Пользователь: user, Домен: example, Расширение: com
```

## Глубокое погружение
Регулярные выражения существуют с 1950-х годов, имея корни в теории автоматов и формальных языков. Модуль `regex` в Rust создан для скорости и безопасности, сосредотачивая внимание на компиляции эффективных шаблонов регулярных выражений во время выполнения. Альтернативы regex включают в себя строковые функции, такие как `find`, `split`, и `replace`, которые покрывают более простые случаи использования без шаблонов. Регулярные выражения в Rust особенно эффективны благодаря обширной оптимизации и компиляции шаблонов регулярных выражений.

## Смотрите также
- Документация крейта `regex`: https://docs.rs/regex/
- Раздел о regex в книге по Rust: https://doc.rust-lang.org/book/ch18-00-patterns.html
- Глава о регулярных выражениях в "Языке программирования Rust": https://doc.rust-lang.org/stable/book/ch18-03-pattern-syntax.html