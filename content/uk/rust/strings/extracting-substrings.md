---
title:                "Виділення підрядків"
aliases:
- /uk/rust/extracting-substrings.md
date:                  2024-01-20T17:46:54.190498-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Витягування підрядків – це процес отримання частини тексту з більшого рядка. Програмісти роблять це, щоб проаналізувати, змінити або просто використати цей конкретний шматок даних.

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
