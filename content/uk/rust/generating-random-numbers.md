---
title:                "Rust: Генерування випадкових чисел"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

Почему: Згенерувати випадкові числа може бути корисно для багатьох програм, таких як ігри, шифрування та статистичні обчислення.

## Як

```Rust
use rand::prelude::*;
use std::io;

fn main() {
    // Задаємо діапазон для випадкового числа
    println!("Введіть мінімальне число: ");
    let mut min = String::new();
    io::stdin().read_line(&mut min).expect("Failed to read input");
    println!("Введіть максимальне число: ");
    let mut max = String::new();
    io::stdin().read_line(&mut max).expect("Failed to read input");
    // Перетворюємо введені значення у числа
    let min: i32 = min.trim().parse().expect("Please enter a number");
    let max: i32 = max.trim().parse().expect("Please enter a number");
    // Генеруємо випадкове число та виводимо його
    let random_num = thread_rng().gen_range(min, max);
    println!("Згенероване випадкове число: {}", random_num);
}
```

Вихід:

```Rust
Введіть мінімальне число: 1
Введіть максимальне число: 10
Згенероване випадкове число: 5
```

## Глибше про генерацію випадкових чисел

Генерація випадкових чисел - важлива частина багатьох програм та алгоритмів. У Расті, для генерації випадкових чисел використовується бібліотека `rand`, яка надає багато різноманітних функцій для генерації чисел, рядків та байтів. Її можна встановити за допомогою менеджера пакетів Cargo: `cargo install rand`.

## Дивитися також

- [Офіційна документація бібліотеки `rand`](https://docs.rs/rand/0.8.3/rand/)
- [Відео-урок з генерації випадкових чисел на мові Раст](https://www.youtube.com/watch?v=sncGOTphiEI)
- [Початковий код для генерації випадкових чисел у Раст](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=2d71939247386d119441ac93d83cb6da)