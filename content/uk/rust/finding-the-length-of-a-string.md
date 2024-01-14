---
title:    "Rust: Знаходження довжини рядка"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Чому

Кожен, хто програмує на Русті, знає, що рядки є невід`ємною частиною багатьох програм. Знаходження довжини рядка є важливою задачею для багатьох програмістів. У цій статті ми розглянемо, як знайти довжину рядка у Русті.

## Як

Зробити це дуже просто, у Русті є вбудована функція `len()`, яка повертає довжину рядка. Подивіться на приклад нижче, як ми використовуємо цю функцію у нашому коді:

```Rust
fn main() {
    let my_string = "Привіт, світе!";
    println!("Довжина рядка: {}", my_string.len());
}
```

Результат виконання цього коду буде: `Довжина рядка: 14`.

Це простий приклад, але можна також виконати інші операції з рядками, використовуючи функцію `len()`. Наприклад, зрізати частину рядка або перевірити, чи містить він певний символ.

## Глибокий занурення

Як ми вже згадували, у Русті є вбудована функція `len()`, яка повертає довжину рядка. Але як це працює під капотом?

У Русті, рядки представлені у вигляді укладеного масиву байтів, який закінчується нульовим байтом. Функція `len()` перебирає елементи масиву до того моменту, коли зустрічає нульовий байт, і повертає кількість елементів до цього моменту.

Також, варто зазначити, що довжина рядка не обов'язково пов'язана з відображальною довжиною. У Русті, буферизовані рядки можуть містити символи, які займають більше одного байту, тому довжина рядка може бути більшою, ніж відображальна довжина.

## See Also

- [Вбудовані функції у Русті](https://doc.rust-lang.org/std/index.html)
- [Керування рядками у Русті](https://www.tutorialspoint.com/rust/rust_strings.htm)
- [Опис більш складних методів для роботи з рядками у Русті](https://rust-unofficial.github.io/too-many-lists/second.html)