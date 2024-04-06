---
date: 2024-01-20 17:46:54.190498-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412\u0438\u0432\u0456\u0434."
lastmod: '2024-04-05T21:53:49.141107-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 6
---

## Як це зробити:
```Rust
fn main() {
    let text = "Вітання з України!";
    let start = text.find('з').expect("Substring not found");
    let end = start + 'з'.len_utf8();
    let substring: &str = &text[start..end];

    println!("The substring is: {}", substring);
}
```

Вивід:
```
The substring is: з
```

Щоб витягти слово "України", робимо так:
```Rust
fn main() {
    let text = "Вітання з України!";
    let start = text.find('У').unwrap_or(0);
    let end = text.len();
    let substring = &text[start..end];

    println!("The substring is: {}", substring);
}
```

Вивід:
```
The substring is: України!
```

## Глибоке занурення
Витягування підрядків у Rust відбувається через зрізи рядків, які беруть початковий і кінцевий індекси. Історично, Rust розвинувся з мови, що наголошує на безпеку пам'яті та паралелізм, тому він використовує сувору систему позицій для уникнення помилок, як-от "index out of bounds".

Є альтернативні способи витягнення підрядків, наприклад, метод `split` для розділення рядка по роздільникам або регулярні вирази з крейта `regex` для більш складних шаблонів.

Деталі реалізації: витягування оперує Unicode скалярами, не байтами, що важливо для коректної обробки текстів, подібних до української мови з її специфічними символами. Невірно вираховані індекси можуть привести до panic за несподіваним збоєм в коді, отже краще користуватися методами `find` чи `chars().nth()` для безпечного доступу до певних символів.

## Дивіться також:
- [The Rust Programming Language – Ch. 4.3. Slices](https://doc.rust-lang.org/book/ch04-03-slices.html)
- [Rust by Example - Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
