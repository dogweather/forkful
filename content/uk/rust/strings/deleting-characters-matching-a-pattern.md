---
title:                "Видалення символів за візерунком"
aliases:
- /uk/rust/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:43:29.622068-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що і Чому?
Видалення символів за візерунком є процесом фільтрації рядка, усуваючи визначені символи або групи символів. Програмісти роблять це для очищення даних, валідації вхідних значень чи просто для приведення тексту до потрібного формату.

## Як це зробити:
```Rust
fn main() {
    let message = "Вітаємо123 у Rust!";
    let pattern = |c: char| c.is_numeric();
    let cleansed_message = delete_chars_matching_pattern(&message, pattern);
    
    println!("Clean message: {}", cleansed_message);
    // Вивід: Clean message: Вітаємо у Rust!
}

fn delete_chars_matching_pattern<F>(s: &str, pattern: F) -> String
where
    F: Fn(char) -> bool,
{
    s.chars().filter(|&c| !pattern(c)).collect()
}
```

## Підводне Каміння
Раніше, в мовах, подібних до C, програмісти часто застосовували ручне управління пам'яттю для видалення символів. В Rust, завдяки власній системі власності та безпечному управлінню пам'яттю, це процес став значно безпечнішим та ефективнішим. Як альтернатива функціональному підходу, є методи стандартної бібліотеки, такі як `replace` або `retain`, хоча вони можуть бути менш гнучкими для складних візерунків. Використання замикань дозволяє Rust програмістам легко налаштувати процес видалення без додаткових витрат на продуктивність.

## Дивіться Також
- [std::str::Chars](https://doc.rust-lang.org/std/str/struct.Chars.html) документація по ітератору символів рядків.
- [std::string::String](https://doc.rust-lang.org/std/string/struct.String.html) документація по типу String у Rust.
- Концепція замикань в Rust: [Rust Book - Closures](https://doc.rust-lang.org/book/ch13-01-closures.html).
