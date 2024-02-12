---
title:                "Перетворення рядка у нижній регістр"
aliases:
- /uk/rust/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:18.879169-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? / Що і навіщо?
Перетворення строки в нижній регістр - це зміна всіх великих літер на малі. Програмісти роблять це для уніфікації даних, полегшення пошуку та порівняння строк.

## How to: / Як це зробити:
```Rust
fn main() {
    let greeting = "Вітаю, Світе!";
    let lower_case_greeting = greeting.to_lowercase();
    println!("{}", lower_case_greeting);
}
```
Вивід:
```
вітаю, світе!
```

## Deep Dive / Поглиблений розбір:
Переведення строки у нижній регістр не новина; ця функціональність існує в багатьох мовах програмування. У Rust, метод `to_lowercase()` враховує локаль та специфіку мови, також правильно обробляє Unicode. Альтернатива - `to_ascii_lowercase()`, яка працює тільки з ASCII символами. Внутрішньо, Rust використовує Unicode database для визначення того, яким має бути нижній регістр для кожного символу.

## See Also / Дивіться також:
- Rust documentation on String methods: [Strings in Rust](https://doc.rust-lang.org/std/string/struct.String.html)
- For understanding Unicode handling in Rust: [Unicode Scalar Values in Rust](https://doc.rust-lang.org/book/ch08-02-strings.html#unicode-scalar-values)
- The ASCII function's documentation: [to_ascii_lowercase()](https://doc.rust-lang.org/std/primitive.str.html#method.to_ascii_lowercase)
