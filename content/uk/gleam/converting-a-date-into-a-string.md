---
title:    "Gleam: Перетворення дати в рядок"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому
Конвертація дати в рядок є важливим завданням для багатьох програм, які працюють з датами та часом. Це допоможе вам у виведенні дати у зрозумілому форматі або збереженні як рядок у базі даних.

## Як
```Gleam
import Gleam.Date

date = Date.make(2021, 11, 28)
formatted_date = Date.to_string(date)
```

У цьому прикладі ми імпортуємо модуль `Gleam.Date` та створюємо об'єкт дати з значеннями року, місяця та дня. Потім ми використовуємо функцію `to_string` для конвертації дати у рядок. Результат буде виглядати як `2021-11-28`.

## Deep Dive
Конвертація дати у рядок в Gleam використовує модуль `Gleam.Date` та функцію `to_string`. Ця функція приймає один аргумент - об'єкт дати типу `Date`. Вона повертає рядок зі зміненим форматом дати, що може бути використаний для виведення або збереження у базі даних. Ви також можете вказати бажаний формат дати за допомогою параметра у функції `to_string`. Наприклад, `Date.to_string(date, "%d-%b-%Y")` виведе дату у форматі `28-Nov-2021`.

## See Also
- [Gleam Date module documentation](https://gleam.run/modules/gleam-date/latest/Date.html)
- [Gleam Date formatting documentation](https://gleam.run/modules/gleam-date/latest/Format.html)