---
title:                "Интерполяция строки"
aliases:
- ru/rust/interpolating-a-string.md
date:                  2024-01-28T23:58:58.923046-07:00
model:                 gpt-4-0125-preview
simple_title:         "Интерполяция строки"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Интерполяция строк включает вставку переменных непосредственно в строки. Это делает построение строк плавным и читаемым, избегая неуклюжих конкатенаций.

## Как это сделать:

В Rust мы используем макрос `format!`:

```Rust
fn main() {
    let name = "Ferris";
    let greeting = format!("Привет, {}!", name);
    println!("{}", greeting); // Выведет "Привет, Ferris!"
}
```
Макрос `format!` работает так же, как `println!`, но возвращает отформатированную строку вместо её вывода.

## Погружение в тему

Rust выбрал макросы вроде `format!` для интерполяции строк вместо синтаксиса внутри языка. Почему? Макросы мощные и гибкие — они расширяют функциональность языка без усложнения синтаксиса.

Исторически, языки вроде C использовали функции вроде `sprintf`, которые были неуклюжими и склонными к ошибкам. Макрос `format!` в Rust безопаснее, предотвращая распространённые ошибки.

Существуют альтернативы, например, конкатенация с использованием `+` или макрос `format_args!` для избежания выделения памяти на куче. Но когда речь идёт о простоте и ясности, `format!` — король.

Замечание о производительности: `format!` выделяет память. Для кода, где критична производительность, рассмотрите другие методы, такие как прямая запись в буфер.

## Смотрите также

- Официальная документация Rust по `format!`: https://doc.rust-lang.org/std/macro.format.html
- Сравнение `format!` и `println!`: https://doc.rust-lang.org/book/ch01-02-hello-world.html
- Rust by Example по форматированию: https://doc.rust-lang.org/rust-by-example/hello/print/print_display.html
