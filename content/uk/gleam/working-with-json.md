---
title:                "Gleam: Робота з json"
simple_title:         "Робота з json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

Робота з JSON є надзвичайно важливою для програмістів, бо цей формат даних дозволяє ефективно обмінюватися інформацією між різними програмами та системами. Завдяки йому, розробникам стає значно простіше створювати зручні та потужні додатки.

## Як зробити

Для початку, ми повинні встановити імпорт "json" у нашому файлі Gleam, щоб мати можливість працювати з цим форматом. Після цього, нам треба буде створити змінну, яка міститиме наші дані у форматі JSON. Отже, у нашому блоку коду ```Gleam на наступній середній можна показати приклад коду, який отримує та виводить дані з JSON:

```Gleam
змінна = Json.decode("""
{
     "name": "John",
     "age" : 30,
     "city": "Kyiv"
}
""")
// Результат: Ok(
// Record(
// (name, String("John")),
// (age, Int(30)),
// (city, String("Kyiv"))
// )
// )
```

## Глибоке дослідження

Для більш розгорнутого розуміння того, як працює формат JSON, варто розглянути інші типи даних, які можна зберігати у цьому форматі, такі як масиви та об'єкти. Крім того, варто розглянути такі поняття, як серіалізація та десеріалізація, які допоможуть управляти даними у форматі JSON з більшою легкістю.

## Дивись також

- [Офіційна документація Gleam для JSON] (https://gleam.run/documentation/standard_library/json/)
- [Відео уроки з програмування на Gleam] (https://www.youtube.com/playlist?list=PLNN7ygYhMImcJZ_zsHFCBYO5MzawDIQEY)
- [Блог про Gleam та інші функціональні мови програмування] (https://xodustech.com/blog/tags/gleam/)