---
title:                "Rust: Перетворення рядка у нижній регістр"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Конвертування рядка в нижній регістр є важливою задачею в програмуванні, оскільки дозволяє зберегти єдність даних і полегшити порівняння рядків. Також це необхідно для коректного відображення тексту на екрані, де капіталізація має велике значення.

## Як

```Rust
let string = "Привіт Світ!";
let lowercase_string = string.to_lowercase();
println!("Рядок в нижньому регістрі: {}", lowercase_string);

// Вивід: рядок в нижньому регістрі: привіт світ!
```

У прикладі вище ми використали метод `to_lowercase ()`, щоб конвертувати рядок в нижній регістр. Цей метод повертає новий рядок зі зміненим регістром.

## Глибоке дослідження

У реалізації методу `to_lowercase()` в Rust використовується стандартна бібліотека Unicode. Вона має визначений список символів, які мають спеціальний нижній регістр. При конвертуванні рядка, ці спеціальні символи розпізнаються і замінюються на відповідні нижній регістр символи.

## Дивіться також

- [Utf8Proc](https://github.com/JuliaStrings/utf8proc) для більш точної обробки Unicode рядків.
- [Порівняння рядків](https://doc.rust-lang.org/1.53.0/book/ch08-02-strings.html#comparing-strings) в Rust.
- [Стандартна бібліотека Unicode в Rust](https://doc.rust-lang.org/std/std/unicode/index.html).