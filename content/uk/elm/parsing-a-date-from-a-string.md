---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому? 

Розбір дати з рядка - це, по суті, перетворення текстового представлення дати в структуру даних, підтримувану мовою програмування. Програмісти роблять це, щоб маніпулювати датами та часом у своїх застосунках більш природньо і ефективно.

## Як це зробити:

```Elm
import Date exposing (Date)
import Time 

parseDate : String -> Date 
parseDate str =
    case Time.fromIsoString str of
        Ok time -> Time.toDate time
        Err msg -> Debug.todo "handle the error here"
```
В цьому коді ми імпортуємо модулі Date та Time. `parseDate` - це функція, яка приймає рядок і перетворює його на дату. `Time.fromIsoString` перевіряє, чи можна час перевести з ISO рядка. У разі успіху ми потім перетворюємо цей час на дату. У випадку помилки ми маємо обробити цю помилку.
              
## Поглиблений огляд:

Розбір дати з рядка існував ще до появи мови Elm. Раніш програмісти використовували складні регулярні вирази для розбору дати, що було помилкою.

У Elm є різні альтернативи, деякі більш гнучкі, а деякі більш строгі, залежно від потреб.

Використовуючи часові зони та інші атрибути, розбір дати з рядка може стати складним. Elm робить цей процес простим, використовуючи модуль Time.

## Дивитися також:

1. [Офіційна документація Elm з модулем Date](https://package.elm-lang.org/packages/elm/time/latest/Time)
2. [Руководство Elm для часу та дати](https://elmprogramming.com/time-and-date.html)
3. [Стаковерфлоу: Розбір дати у Elm](https://stackoverflow.com/questions/48257896/date-and-time-in-elm)