---
date: 2024-01-26 04:45:55.917023-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Rust \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\
  \u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438 \u043A\
  \u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u0438\u0445 \u0447\u0438\u0441\u0435\
  \u043B, \u0430\u043B\u0435 \u0442\u0430\u043A\u0456 \u043A\u043E\u043D\u0442\u0435\
  \u0439\u043D\u0435\u0440\u0438 \u044F\u043A `num-complex` \u043F\u0440\u0438\u0439\
  \u0434\u0443\u0442\u044C \u043D\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\
  \u0443. \u041E\u0441\u044C \u044F\u043A \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0439\u043E\u0433\u043E."
lastmod: '2024-03-13T22:44:48.926522-06:00'
model: gpt-4-0125-preview
summary: "Rust \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438\
  \ \u043A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u0438\u0445 \u0447\u0438\
  \u0441\u0435\u043B, \u0430\u043B\u0435 \u0442\u0430\u043A\u0456 \u043A\u043E\u043D\
  \u0442\u0435\u0439\u043D\u0435\u0440\u0438 \u044F\u043A `num-complex` \u043F\u0440\
  \u0438\u0439\u0434\u0443\u0442\u044C \u043D\u0430 \u0434\u043E\u043F\u043E\u043C\
  \u043E\u0433\u0443."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u0438\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
weight: 14
---

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
