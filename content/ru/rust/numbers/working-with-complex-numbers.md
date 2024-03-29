---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:38.304501-07:00
description: "\u041A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0435 \u0447\
  \u0438\u0441\u043B\u0430 \u0438\u043C\u0435\u044E\u0442 \u0434\u0435\u0439\u0441\
  \u0442\u0432\u0438\u0442\u0435\u043B\u044C\u043D\u0443\u044E \u0438 \u043C\u043D\
  \u0438\u043C\u0443\u044E \u0447\u0430\u0441\u0442\u0438 \u0438 \u0438\u0433\u0440\
  \u0430\u044E\u0442 \u0432\u0430\u0436\u043D\u0443\u044E \u0440\u043E\u043B\u044C\
  \ \u0432 \u0440\u0430\u0437\u043B\u0438\u0447\u043D\u044B\u0445 \u043E\u0431\u043B\
  \u0430\u0441\u0442\u044F\u0445, \u0442\u0430\u043A\u0438\u0445 \u043A\u0430\u043A\
  \ \u0438\u043D\u0436\u0435\u043D\u0435\u0440\u0438\u044F, \u0444\u0438\u0437\u0438\
  \u043A\u0430 \u0438 \u043A\u043E\u043C\u043F\u044C\u044E\u0442\u0435\u0440\u043D\
  \u0430\u044F \u0433\u0440\u0430\u0444\u0438\u043A\u0430.\u2026"
lastmod: '2024-03-13T22:44:44.654765-06:00'
model: gpt-4-0125-preview
summary: "\u041A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0435 \u0447\
  \u0438\u0441\u043B\u0430 \u0438\u043C\u0435\u044E\u0442 \u0434\u0435\u0439\u0441\
  \u0442\u0432\u0438\u0442\u0435\u043B\u044C\u043D\u0443\u044E \u0438 \u043C\u043D\
  \u0438\u043C\u0443\u044E \u0447\u0430\u0441\u0442\u0438 \u0438 \u0438\u0433\u0440\
  \u0430\u044E\u0442 \u0432\u0430\u0436\u043D\u0443\u044E \u0440\u043E\u043B\u044C\
  \ \u0432 \u0440\u0430\u0437\u043B\u0438\u0447\u043D\u044B\u0445 \u043E\u0431\u043B\
  \u0430\u0441\u0442\u044F\u0445, \u0442\u0430\u043A\u0438\u0445 \u043A\u0430\u043A\
  \ \u0438\u043D\u0436\u0435\u043D\u0435\u0440\u0438\u044F, \u0444\u0438\u0437\u0438\
  \u043A\u0430 \u0438 \u043A\u043E\u043C\u043F\u044C\u044E\u0442\u0435\u0440\u043D\
  \u0430\u044F \u0433\u0440\u0430\u0444\u0438\u043A\u0430.\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u044B\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
---

{{< edit_this_page >}}

## Что и Почему?
Комплексные числа имеют действительную и мнимую части и играют важную роль в различных областях, таких как инженерия, физика и компьютерная графика. Программисты используют их для решения уравнений, с которыми обычные действительные числа не справляются.

## Как это сделать:
В Rust нет встроенной поддержки комплексных чисел, но такие крейты, как `num-complex`, помогут вам. Вот как это использовать:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let sum = a + b;
    let product = a * b;

    println!("Сумма: {}", sum); // Сумма: 3 - 1i
    println!("Произведение: {}", product); // Произведение: 14 - 5i
}
```
Вам нужно будет добавить `num_complex` в ваш `Cargo.toml`, чтобы заставить это работать.

## Погружение
Комплексные числа были предложены в 16 веке, но настоящий интерес к ним возник в 18 веке, когда математики, такие как Эйлер, начали с ними экспериментировать.

Без встроенных операций с комплексными числами языки, такие как Rust, полагаются на сторонние библиотеки. `num-complex` - это один из таких крейтов и является частью коллекции крейтов `num`, которая стремится предоставить числовые типы и трейты для Rust.

Стоит отметить, что некоторые языки (например, Python) имеют встроенную поддержку комплексных чисел, в то время как другие (например, C++, с заголовочным файлом `<complex>`) предоставляют их как часть стандартной библиотеки. В Rust решение держать стандартную библиотеку небольшой означает, что часто придется обращаться к крейтам, созданным сообществом, за дополнительными функциональностями.

## Смотрите также
- [Книга по Rust](https://doc.rust-lang.org/book/): Чтобы узнать больше о Rust и как работать с внешними крейтами.
- [Комплексные числа на Википедии](https://en.wikipedia.org/wiki/Complex_number): Для более глубокого понимания комплексных чисел.
