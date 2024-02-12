---
title:                "Робота з комплексними числами"
date:                  2024-01-26T04:45:55.917023-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з комплексними числами"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?
Комплексні числа мають дійсну та уявну частини і є необхідними в різних областях, таких як інженерія, фізика та комп'ютерна графіка. Програмісти використовують їх для розв'язання рівнянь, з якими звичайні дійсні числа не можуть впоратися.

## Як це зробити:
Rust не має вбудованої підтримки комплексних чисел, але такі контейнери як `num-complex` прийдуть на допомогу. Ось як використовувати його:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let sum = a + b;
    let product = a * b;

    println!("Сума: {}", sum); // Сума: 3 - 1i
    println!("Добуток: {}", product); // Добуток: 14 - 5i
}
```
Вам потрібно додати `num_complex` до вашого `Cargo.toml`, щоб це чудо сталося.

## Поглиблене вивчення
Комплексні числа були придумані у 16 столітті, але насправді набрали обертів у 18 столітті, коли математики, на кшталт Ейлера, почали з ними експериментувати.

Без вбудованих операцій з комплексними числами мови, на кшталт Rust, покладаються на сторонні бібліотеки. `num-complex` є одним з таких контейнерів і є частиною колекції контейнерів `num`, яка прагне забезпечити числові типи та властивості для Rust.

Варто згадати, що деякі мови (як-от Python) мають вбудовану підтримку комплексних чисел, тоді як інші (як-от C++, з заголовком `<complex>`) надають їх як частину стандартної бібліотеки. У Rust рішення тримати стандартну бібліотеку невеликою означає, що ви часто будете вдаватись до створених спільнотою контейнерів для додаткової функціональності.

## Див. також
- [Книга Rust](https://doc.rust-lang.org/book/): Щоб дізнатися більше про Rust і як працювати з зовнішніми контейнерами.
- [Wikipedia Комплексних чисел](https://en.wikipedia.org/wiki/Complex_number): Для глибшого розуміння комплексних чисел.