---
title:                "Приголомшення рядка"
html_title:           "Rust: Приголомшення рядка"
simple_title:         "Приголомшення рядка"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizing a String in Rust 

## Що це та навіщо це потрібно?

Коли ми говоримо про піднесення рядка до великої букви, ми маємо на увазі перетворення першої букви рядка в верхній регістр. Програмісти роблять це, наприклад, для нормалізації вводу даних користувачів або для відповідності стандартам відображення тексту.

## Як це зробити:

Rust не має вбудованої функції для роботи із рядками, тому нам потрібно створити власну. Давайте подивимося, як це можна зробити:

```Rust
fn capitalize_first(input: &str) -> String {
    let mut c = input.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_str = "hello world";
    println!("{}", capitalize_first(my_str));
}
```

При виконанні цього коду вивід буде таким: `Hello world`

## Глибше занурення

Піднесення першої букви рядка до верхнього регістру - це практика, що сягає коріннями часів друкарської діяльності. 

Для альтернативних підходів, можна використовувати бібліотеки сторонніх розробників, які надають більш широкий функціонал роботи із рядками, такі як `titlecase`.

Деталі виконання функції `capitalize_first` включають ітерування через символи вхідного рядка та додавання їх до нового рядка після перетворення першого символу до верхнього регістру.

## Дивитись також

- [Rust String документація](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust Char методи](https://doc.rust-lang.org/std/primitive.char.html)
- [Бібліотека `titlecase` в Crates.io](https://crates.io/crates/titlecase)