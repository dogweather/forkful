---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Визначення довжини рядка - це вимірювання кількості символів в ньому. Програмісти це роблять, щоб оптимізувати виконання завдань, пов'язаних із маніпуляціями з рядками.

## Як це зробити:

Використовуємо метод `.len()` у Rust. 

```rust
fn main() {
   let my_string = "Привіт, світ!";
   println!("{}", my_string.len());
}

```
Коли ми запускаємо цей код, ми отримуємо наступний вихід:

```
13
```

## Поглиблений огляд:

Історично, у багатьох мовах програмування для вимірювання довжини рядка використовувалися цикли. Однак Rust пропонує вбудований метод `.len()`, що робить цю роботу швидше і легше.

Існує альтернатива - метод `.chars().count()`, але він працює повільніше, оскільки враховує юнікод-символи.

Особливість методу `.len()` полягає в тому, що він повертає кількість байтів, а не символів. Це може призвести до неточностей, коли ми працюємо з рядками, які включають юнікод-символи.

## Дивіться також:

1. Документація Rust по методу `.len()`: [Rust .len()](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
2. Документація Rust по методу `.chars().count()`: [Rust .chars()](https://doc.rust-lang.org/std/string/struct.String.html#method.chars)
3. Обговорення на форумі StackOverflow про різницю між `.len()` і `.chars().count()`: [StackOverflow discussion](https://stackoverflow.com/questions/26990432/rust-string-length-in-characters)