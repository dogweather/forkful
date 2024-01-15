---
title:                "Початок нового проєкту"
html_title:           "Gleam: Початок нового проєкту"
simple_title:         "Початок нового проєкту"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Чому

Початок нового проекту може бути захопливою та цікавою пригодою. За допомогою Gleam, ви можете розробити ефективні та надійні програми з чистою та простою синтаксичною структурою.

## Як

```Gleam
родичний модуль gleam/main

pub fn main(name: String) {
  let greeting = "Привіт".concat(name)
  Gleam.IO.print_line(greeting)
}
```

Використання модуля `gleam/main` дозволяє створити основу для вашої програми. Ви можете використовувати функцію `main` для запуску програми та передавати параметри. У цьому прикладі, ми використовуємо функцію `concat` для створення привітання з іменем, яке буде передане програмі.

## Глибоке занурення

Початок нового проекту може бути завданням, що запускає багато питань. Тут ми розглянемо кілька важливих кроків, які допоможуть вам розпочати проект за допомогою Gleam.

- Перевірте, чи ви користуєтеся останньою версією Gleam за допомогою команди `gleam -v`.
- Ви можете створити новий проект за допомогою команди `gleam new <ім'я проекту>`.
- Для створення нового модуля використовуйте команду `gleam new module <ім'я модуля>`.

## Дивіться також

- [Документація Gleam](https://gleam.run/docs/getting-started/installation)
- [Загальна інформація про програмування з Gleam](https://gleam.run/docs/guide)
- [Приклади програм на Gleam](https://github.com/gleam-lang/gleam/tree/master/examples)