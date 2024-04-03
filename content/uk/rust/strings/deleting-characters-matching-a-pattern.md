---
date: 2024-01-20 17:43:29.622068-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : ."
lastmod: '2024-03-13T22:44:48.910473-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\u0432\
  \u043E\u043B\u0456\u0432 \u0437\u0430 \u0432\u0456\u0437\u0435\u0440\u0443\u043D\
  \u043A\u043E\u043C"
weight: 5
---

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
