---
title:                "Перетворення рядка на нижній регістр"
html_title:           "Rust: Перетворення рядка на нижній регістр"
simple_title:         "Перетворення рядка на нижній регістр"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Існує безліч ситуацій, коли нам потрібно перетворити текст на прописні чи строчні літери. Найбільш очевидна причина - валідація введеного користувачем паролю.

## Як це зробити

Ми можемо використовувати метод `.to_lowercase()` для рядка, який поверне новий рядок із всіма буквами в нижньому регістрі.

```Rust
let text = "HELLO WORLD";
let lowercase_text = text.to_lowercase();
println!("{}", lowercase_text); // виведе "hello world"
```

## Глибока пірнатя

У Rust, кожен символ є окремим типом даних. Це означає, що ми не можемо просто замінити прописні букви на строчні. Замість цього, ми повинні перетворювати кожен символ окремо. Також, використовувати метод `.to_lowercase()` може призвести до неправильного результату для деяких мов, де є більше, ніж одна версія тієї ж літери (наприклад, турецька або грецька).

## Дивіться також

- [Резики бібліотеки `unicode-normalization`](https://crates.io/crates/unicode-normalization)
- [Документація для методу `.to_lowercase()`](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Стаття про обробку рядків в Rust](https://www.lpalmieri.com/posts/2021-03-13-rust-processing-strings/)