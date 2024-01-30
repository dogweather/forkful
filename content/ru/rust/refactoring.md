---
title:                "Рефакторинг"
date:                  2024-01-29T00:02:16.261080-07:00
model:                 gpt-4-0125-preview
simple_title:         "Рефакторинг"
programming_language: "Rust"
category:             "Rust"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/refactoring.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Рефакторинг - это процесс реструктуризации существующего компьютерного кода — изменения факторинга — без изменения его внешнего поведения. Программисты делают это для улучшения нефункциональных атрибутов программного обеспечения, таких как читаемость, снижение сложности, повышение удобства обслуживания и создание более выразительной внутренней архитектуры или объектной модели для улучшения расширяемости.

## Как сделать:

Давайте отрефакторим простой фрагмент кода на Rust, чтобы сделать его более идиоматичным и удобным для поддержки. Начнем с функции, которая вычисляет сумму вектора целых чисел:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("Сумма равна {}", sum(&numbers));
}
```

Вывод:
```
Сумма равна 15
```

Теперь давайте отрефакторим это, используя более идиоматичный Rust, применяя итераторы и метод `fold`:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("Сумма равна {}", sum(&numbers));
}
```

Вывод без изменений — все равно `15` — но отрефакторенная версия чище и использует сильные стороны Rust, такие как заимствование и методы итераторов.

## Глубокое погружение

Рефакторинг берет свои корни в сообществе Smalltalk и был популяризирован в мире Java благодаря книге Мартина Фаулера "Рефакторинг: Улучшение проекта существующего кода". Его принципы универсальны и применимы к Rust, где безопасность и параллелизм имеют первостепенное значение. Rust поощряет написание надежного кода, обнаруживая проблемы во время компиляции, так что во время рефакторинга компилятор Rust служит сетью безопасности.

Альтернативы ручному рефакторингу включают использование автоматизированных инструментов, таких как 'rustfmt' для форматирования кода и 'clippy' для линтинга, которые могут предложить более идиоматичные способы написания кода. Однако глубокий рефакторинг часто требует осмысленного понимания дизайна кода, что эти инструменты не могут полностью автоматизировать.

В Rust рефакторинг может касаться улучшения использования типов, эффективного использования времен жизни, сокращения ненужных аллокаций или применения паттернов параллелизма, таких как использование `Arc<Mutex<T>>` при необходимости. Также распространено переход от `unwrap()` к более выразительной обработке ошибок с `Result<T, E>`.

## Смотрите также

Чтобы углубленно изучить рефакторинг в Rust:

- Книга по Rust: https://doc.rust-lang.org/book/
- Rust на практике: https://doc.rust-lang.org/rust-by-example/
- Clippy, инструмент для линтинга Rust: https://github.com/rust-lang/rust-clippy
- "Рефакторинг: Улучшение проекта существующего кода" от Мартина Фаулера: https://martinfowler.com/books/refactoring.html