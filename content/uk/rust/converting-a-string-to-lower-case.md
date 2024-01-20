---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Elixir: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і чому?
Конвертація рядка в нижній регістр – це процес перетворення всіх літер рядка в нижній регістр. Програмісти роблять це для полегшення порівняння рядків, зниження помилок даних і нормалізації вводу. 

## Як це робиться: 
```Rust
let str = "Hello, World!";
let lower_str = str.to_lowercase();
println!("{}", lower_str);
```
Вивід: 
```Rust
hello, world!
```
## Глибше в тему
Перетворення рядка в нижній регістр - не нова ідея, та вона активно використовується в багатьох мовах програмування. Більше того, `to_lowercase` в Rust використовує Unicode Scalar Value, що є більш інтуїтивним для користувачів, які мають різні мовні налаштування. 

Є кілька альтернативних методів конвертації рядків в нижній регістр, наприклад, використання ASCII. Однак, `to_lowercase` вміє працювати з більшим розмаїттям символів, включаючи спеціальні символи і букви з діакритичними знаками. 

Реалізація `to_lowercase` в Rust також надає перевагу в якості компілювання, що робить цей метод ефективним і надійним для виконання своїх задач. 

## Дивіться також
- [Rust String](https://doc.rust-lang.org/std/string/struct.String.html) 
- [Unicode Scalar Value](https://doc.rust-lang.org/std/primitive.char.html)
- [Rust to_lowercase](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)