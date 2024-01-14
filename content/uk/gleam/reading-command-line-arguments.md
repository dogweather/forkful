---
title:                "Gleam: Читання аргументів командного рядка"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Чому

Написання програм за допомогою командного рядка є необхідною навичкою для будь-якого програміста. Це дає можливість працювати з певними параметрами без необхідності змінювати вихідний код. Це зробить ваш код більш гнучким і легко змінюваним.

##Як

Зчитування вхідних аргументів через командний рядок в Gleam є простим і зрозумілим. Для цього використовуйте функцію `command_line.arguments()`, яка повертає список аргументів, переданих програмі при запуску. Наприклад, якщо ви запускаєте програму з наступним кодом:

```Gleam
import gleam/io

fn main() {
  let args = command_line.arguments()
  let name = list.nth_or_default(args, 0, "world")
  io.print("Hello, #{name}")
}
```

Ви можете передати вхідний аргумент `John` і побачити наступний результат: `Hello, John`.

##Глибокий занурення

Крім зчитування вхідних аргументів, в Gleam також є можливість передавати аргументи для виконання конкретних дій. Наприклад, ви можете визначити вхідний аргумент `add`, який буде приймати два числа і повертати їх суму. Тоді ваш код може виглядати наступним чином:

```Gleam
fn main() {
  let args = command_line.arguments()
  case list.nth(args, 0) {
    () -> io.print("Please provide a command")
    "add" ->
      let num_1 = argv:get(args, "num_1")
      let num_2 = argv:get(args, "num_2")
      io.print("The sum of #{num_1} and #{num_2} is #{num_1 + num_2}")
    _ -> io.print("Invalid command")
  }
}
```

І при запуску програми з наступними аргументами: `add --num_1 5 --num_2 10`, ви отримаєте вивід `The sum of 5 and 10 is 15`.

##Дивіться також

- Офіційна документація Gleam про [читання командних аргументів](https://gleam.run/book/tour/command_line_arguments.html)
- Приклади використання функції `command_line.arguments()` на [Github](https://github.com/search?q=command_line.arguments%28%29+language%3AGleam&type=Code)
- Блог про програмування на Gleam [Gleamverse](https://gleamverse.com/)

[Оригінал статті](https://gleam.run/blog/reading-command-line-arguments.html)