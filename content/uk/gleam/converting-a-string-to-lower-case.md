---
title:                "Gleam: Перетворення рядка на нижній регістр"
simple_title:         "Перетворення рядка на нижній регістр"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому
Іноді в програмуванні нам потрібно змінити регістр рядка на менший, щоб зробити його більш читабельним або виконати деякі функції. У цьому дописі ми розглянемо, як повернути рядок в нижньому регістрі за допомогою мови програмування Gleam.

## Як
```Gleam
  my_string = "Привіт, Світе!"
  new_string = String.to_lower(my_string)
  IO.println(new_string)
```
Вивід: привіт, світе!

## Глибоке дослідження
Переведення рядка в нижній регістр - це процес заміни великих літер на відповідні маленькі літери. У мові Gleam існує вбудована функція String.to_lower, яка автоматично робить це за нас. Однак, ця функція може несподівано поводить себе з знаками пунктуації та буквами з акцентом. Якщо потрібна більш точна конвертація, можна використовувати бібліотеку Chrysaor для роботи з рядками.

## Дивіться також
- [Документація з рядками у мові Gleam](https://gleam.run/documentation/strings/)
- [Бібліотека Chrysaor для роботи з рядками](https://gitlab.com/gleam-lang/chrysaor)
- [Програмування на мові Gleam - початковий посібник](https://dev.to/benjaminpigott/getting-started-with-gleam-the-simpler-faster-erlang-1mn4)