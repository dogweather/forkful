---
title:                "Gleam: Розпочаття нового проекту"
simple_title:         "Розпочаття нового проекту"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Для чого
Початок нового проекту - це завжди захоплива подія для програмістів. Це дає можливість використати свої навички та створити щось нове та цікаве. Крім того, програмування на Gleam може принести нові виклики та розширити свій багаж знань.

## Як це зробити
Для початку нового проекту на Gleam потрібно виконати кілька кроків.

1. Необхідно встановити Gleam за допомогою менеджера пакетів чи вибрати одну з альтернативних опцій установки.
2. Створіть новий проект, використовуючи команду `gleam new [ім'я проекту]`.
3. Змініть поточний каталог на ім'я проекту за допомогою команди `cd [ім'я проекту]`.
4. Редагуйте файл `src/app.gleam` для побудови своєї програми.

### Приклад

```Gleam
import gleam/list

pub struct Person(name: String, age: Int)

testdata : List(Person) =
  [ Person("John", 28), Person("Lena", 25), Person("Max", 32) ]

pub fn under_thirty(people : List(Person)) {
  List.filter(person) people if person.age < 30
}

pub fn log_names(people : List(Person)) {
  List.map(person) people (println(.) person.name)
}

pub fn main() {
  let young_people = under_thirty(testdata)
  log_names(young_people)
}
```

Вивід:

```
John
Lena
```

## Поглиблене вивчення
Створення нового проекту на Gleam може викликати питання щодо архітектури програми та використання різних бібліотек. Щоб отримати більш детальну інформацію про початок нового проекту, радимо переглянути офіційну документацію [Gleam](https://gleam.run/).

## Дивись також
- [Початок роботи з Gleam](https://gleam.run/getting-started/)
- [Документація щодо мови програмування Gleam](https://gleam.run/language/)
- [Приклади проектів на Gleam](https://gleam.run/examples/)