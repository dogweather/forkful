---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і де?

Перетворення дати в рядок - це процес, за допомогою якого ми змінюємо дату в текстовий формат. Програмісти роблять це для простоти відображення та обробки даних.

## Як це зробити:

У мові програмування Gleam це можна реалізувати за допомогою вбудованих функцій. Ось приклад коду:

```Gleam
import gleam/date
import gleam/int.{to_string}

fn datetostring(mydate: date.Date) -> String {
  let year = to_string(date.year(mydate))
  let month = to_string(date.month(mydate))
  let day = to_string(date.day(mydate))
  year ++ "-" ++ month ++ "-" ++ day
}

fn main() {
  let mydate = date.new(2021, 12, 3)
  let mydate_str = datetostring(mydate)
  mydate_str
}
```

Вихідний код буде виглядати так: `"2021-12-3"`

## Поглиблено:

1. В історичному контексті перетворення дати в рядок дозволяло програмістам реалізовувати системи, що могли легко відстежувати та обговорювати події в часі.

2. Альтернативи включают в себе зберігання дат на бітовому рівні або як числа. Однак ці методи менш зручні для людського сприйняття і обробки.

3. Gleam використовує конкатенацію рядків та вбудовані функції для перетворення числових значень року, місяця та дня в рядки, а потім об'єднує їх в один рядок дати.

## Дивіться також:

- [Докладніше про вбудовані функції дати в Gleam](https://gleam.run/book/tour/basic-types.html#date-time)
- [Офіційна документація Gleam](https://gleam.run/book/)
- [Відеокурси про Gleam](https://www.youtube.com/playlist?list=PLh_thOjTnxi46lTgEvJIK5pSPC-aahjLZ)