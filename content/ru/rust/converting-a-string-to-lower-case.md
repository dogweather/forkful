---
title:                "Преобразование строки в нижний регистр"
date:                  2024-01-28T23:57:02.187968-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в нижний регистр"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Преобразование строки в нижний регистр означает превращение каждой буквы в строке в маленькую букву. Это полезно для сравнений, нечувствительных к регистру, или для подготовки текста к единообразной обработке.

## Как это сделать:
```Rust
fn main() {
    let greeting = "HeLLo, WoRlD!";
    let lowercase_greeting = greeting.to_lowercase();
    println!("{}", lowercase_greeting); // "hello, world!"
}
```
Вывод:
```
hello, world!
```

## Подробнее
До метода `.to_lowercase()` разработчики на Rust могли использовать `.to_ascii_lowercase()` для той же задачи, который влиял только на символы ASCII. Стандартная библиотека Rust развивалась, предлагая `.to_lowercase()` для полной поддержки Юникода — это означает, что он может обрабатывать не только английский! Это очень важно, если ваше приложение выходит на более широкую, многоязычную аудиторию.

Что под капотом? На самом деле, метод `to_lowercase()` не просто заменяет 'A' на 'a'. Это скорее маленький лингвист, знающий все о Юникоде. Он следует стандарту Юникода, чтобы правильно преобразовать символы в нижний регистр с учетом их культурных нюансов.

Конечно, есть альтернативы. Вы можете запустить цикл, пройтись по каждому символу и преобразовать его самостоятельно. Но зачем изобретать велосипед, когда стандартная библиотека Rust уже проделала всю работу?

## Смотрите также
- [Документация Rust по `to_lowercase()`](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- [Документация Rust по строкам](https://doc.rust-lang.org/std/string/struct.String.html)
- [Преобразование регистра в Юникоде](https://www.unicode.org/reports/tr21/tr21-5.html)
