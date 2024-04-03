---
date: 2024-01-20 17:39:18.879169-07:00
description: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u043D\u0438\u0436\u043D\u0456\u0439\
  \ \u0440\u0435\u0433\u0456\u0441\u0442\u0440 - \u0446\u0435 \u0437\u043C\u0456\u043D\
  \u0430 \u0432\u0441\u0456\u0445 \u0432\u0435\u043B\u0438\u043A\u0438\u0445 \u043B\
  \u0456\u0442\u0435\u0440 \u043D\u0430 \u043C\u0430\u043B\u0456. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\
  \u044C \u0446\u0435 \u0434\u043B\u044F \u0443\u043D\u0456\u0444\u0456\u043A\u0430\
  \u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445, \u043F\u043E\u043B\u0435\u0433\
  \u0448\u0435\u043D\u043D\u044F \u043F\u043E\u0448\u0443\u043A\u0443 \u0442\u0430\
  \ \u043F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F\u2026"
lastmod: '2024-03-13T22:44:48.915427-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u043D\u0438\u0436\u043D\u0456\u0439\
  \ \u0440\u0435\u0433\u0456\u0441\u0442\u0440 - \u0446\u0435 \u0437\u043C\u0456\u043D\
  \u0430 \u0432\u0441\u0456\u0445 \u0432\u0435\u043B\u0438\u043A\u0438\u0445 \u043B\
  \u0456\u0442\u0435\u0440 \u043D\u0430 \u043C\u0430\u043B\u0456."
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
weight: 4
---

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
