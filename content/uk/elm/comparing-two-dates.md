---
title:                "Порівняння двох дат"
html_title:           "Elm: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Порівняння двох дат - це процес визначення, які з двох дат є раніше, а які пізніше. Це часто потрібно в програмуванні для здійснення логічних операцій і обробки даних.

## Як це зробити:

Використовуючи вбудовані функції для роботи з датами, можна легко порівняти дві дати в Elm. Один з можливих способів - це використовувати функцію `compare`, яка повертає тип `Date.Compare`, який може бути `LT` (менше), `EQ` (рівне) або `GT` (більше). Наприклад:

```Elm
import Date exposing (compare, fromString)

date1 = fromString "2020-01-01"
date2 = fromString "2021-01-01"

result = compare date1 date2 
```

Після виконання цього коду, `result` буде мати значення `LT`, оскільки `date1` є раніше, ніж `date2`. Зверніть увагу, що дати потрібно перетворити в тип `Date` за допомогою функції `fromString`.

## Глибоке занурення

Цей спосіб порівняння дат був впроваджений в Еlm в версії 0.19. До цього, порівнювати дати було не зовсім просто і потребувало залучення додаткових бібліотек. Також варто зазначити, що цей метод працює тільки з датами, а не з дато-часовими об'єктами.

Альтернативним методом порівняння може бути використання функції `comparableDate` з бібліотеки `elm/time`, яка повертає тип `Comparable`. В такому разі, порівняння буде виглядати наступним чином:

```Elm
import Time exposing (..)
import Time.Date as Date exposing ( comparableDate )

date1 = fromString "2020-01-01"
date2 = fromString "2021-01-01"

comparableDate1 = comparableDate date1
comparableDate2 = comparableDate date2

result = comparableDate1 < comparableDate2 
```

Цей метод дозволяє порівнювати не тільки дати, але і дато-часові об'єкти.

## Дивіться також:

Офіційна документація про роботу з датами в Elm: https://package.elm-lang.org/packages/elm/time/latest/Time