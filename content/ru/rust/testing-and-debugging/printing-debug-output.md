---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:17.167288-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u043B\u044F \u0432\u044B\u0432\u043E\u0434\u0430 \u0447\u0435\
  \u0433\u043E-\u0442\u043E \u043F\u0440\u043E\u0441\u0442\u043E\u0433\u043E \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 `println!`. \u0415\u0441\
  \u043B\u0438 \u0432\u0430\u043C \u043D\u0443\u0436\u043D\u043E \u0432\u044B\u0432\
  \u0435\u0441\u0442\u0438 \u0437\u043D\u0430\u0447\u0435\u043D\u0438\u0435 \u0434\
  \u043B\u044F \u043E\u0442\u043B\u0430\u0434\u043A\u0438, `dbg!` \u043E\u043A\u0430\
  \u0436\u0435\u0442\u0441\u044F \u043E\u0447\u0435\u043D\u044C \u043A\u0441\u0442\
  \u0430\u0442\u0438."
lastmod: '2024-03-13T22:44:44.670711-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0432\u044B\u0432\u043E\u0434\u0430 \u0447\u0435\u0433\
  \u043E-\u0442\u043E \u043F\u0440\u043E\u0441\u0442\u043E\u0433\u043E \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 `println!`."
title: "\u0412\u044B\u0432\u043E\u0434 \u043E\u0442\u043B\u0430\u0434\u043E\u0447\u043D\
  \u043E\u0439 \u0438\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0438\u0438"
weight: 33
---

## Как это сделать:
Для вывода чего-то простого используйте `println!`. Если вам нужно вывести значение для отладки, `dbg!` окажется очень кстати.

```Rust
fn main() {
    let mut vec = vec![1, 2, 3];
    
    // Базовый вывод
    println!("Привет, растафарианцы!");

    // Форматирование отладочного вывода с помощью println! используя `{:?}`
    println!("{:?}", vec);

    // Отладка с `dbg!`, выводит в stderr и возвращает значение
    dbg!(&vec);

    // Изменяем vec после использования `dbg!`
    vec.push(4);
    dbg!(vec);
}
```

Пример вывода:

```
Привет, растафарианцы!
[1, 2, 3]
[src/main.rs:9] &vec = [
    1,
    2,
    3,
]
[src/main.rs:13] vec = [
    1,
    2,
    3,
    4,
]
```

## Глубокое погружение
Вывод отладочной информации был простой и прямолинейной частью программирования с самых ранних дней. Его простота часто делает его предпочитаемым выбором для быстрой диагностики проблем.

В Rust, `println!` отлично подходит для отображения сообщений, понятных пользователю. Магия начинается с `dbg!`, введенного в Rust 1.32, который выводит как значение, так и его местоположение в коде. Вывод происходит в стандартный поток ошибок (stderr), поэтому он не смешивается со стандартным выводом (stdout) и может быть перенаправлен отдельно, если это необходимо.

Для сложных типов вы можете вывести трейт `Debug`, чтобы автоматически создать формат, который `println!` и `dbg!` могут использовать. Вот что делает аннотация `#[derive(Debug)]` над вашими структурами и перечислениями.

Что касается альтернатив, существуют специализированные системы логирования, такие как `log` и `env_logger`, и если вам нужен более детализированный контроль, рассмотрите возможность использования отладчика, такого как `gdb` или `lldb`, которые работают с Rust через интеграции вроде `rust-gdb` или `rust-lldb`.

## Смотрите также
Для дополнительной информации о выводе отладочной информации и форматировании в Rust:

- Книга по Rust о `println!` и Форматировании: https://doc.rust-lang.org/std/fmt/index.html
- Документация макроса `dbg!`: https://doc.rust-lang.org/std/macro.dbg.html
- Официальное руководство по отладке с `gdb` и `lldb`: https://rust-lang.github.io/rustup-components-history
- Крейт `log` для более структурированного подхода к логированию: https://crates.io/crates/log
- Крейт `env_logger`, распространенная реализация логгера для фасада `log`: https://crates.io/crates/env_logger
