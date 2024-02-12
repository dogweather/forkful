---
title:                "Визначення довжини рядка"
aliases: - /uk/rust/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:13.800098-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
Знаходження довжини рядка вимірює кількість символів у ньому. Програмісти це роблять, щоб контролювати введення даних, управляти текстом чи порівнювати строки.

## Як це зробити:
```Rust
fn main() {
    let greeting = "Привіт, світ!";
    let length = greeting.chars().count(); // лічить кожен Unicode символ
    
    println!("Довжина рядка: {}", length);
}
```
Вивід:
```
Довжина рядка: 13
```

## Поглиблений огляд
У Rust, рядки - це колекції байтів. Стандартно, `len()` повертає кількість байтів, а не символів. Але оскільки Rust кодує строки в UTF-8, один символ може використовувати від 1 до 4 байтів. Тому `chars().count()` - це спосіб дізнатись довжину строки в символах Unicode.

Іншими словами, `len()` може дати невірні результати для не-ASCII символів. Звернути увагу на `graphemes()` з крейту `unicode-segmentation`, якщо потрібно рахувати графеми, особливо для багатосимвольних клітинок.

Раніше, у мовах програмування що працювали тільки з ASCII, довжина строки була просто кількістю байтів. Rust поширює це на Unicode, встановлюючи зручність і точність у роботі з міжнародним текстом.

## Дивіться також
- Документація по методу `len()` для типу String у Rust: [std::string::String](https://doc.rust-lang.org/stable/std/string/struct.String.html#method.len)
- Офіційна книга Rust про строки: ["The Rust Programming Language" - Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
