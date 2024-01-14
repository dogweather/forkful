---
title:    "Elm: Порівняння двох дат"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Тому заквітчала Elm: Порівняння дат

В програмуванні часто потрібно порівнювати дати, наприклад, для того, щоб визначити, яка з них більша або чи дві дати попадають в один день. Elm має вбудовану функцію для порівняння дат, що робить це завдання простим і зручним.

## Як це зробити:

У Elm для порівняння дат використовується функція `Date.compare`, яка приймає два параметри типу `Date` і повертає один з трьох результатів: `LT`, `EQ` або `GT`. Ось приклад використання цієї функції:

```elm
firstDate = Date.fromString "2021-07-15"
secondDate = Date.fromString "2021-07-20"

result = Date.compare firstDate secondDate

-- результат: GT
```

Функція `Date.fromString` конвертує рядок у тип `Date`, що дозволяє легко проводити операції з датами. Для отримання більш детальної інформації щодо роботи з датами у Elm, можна ознайомитися з [документацією](https://package.elm-lang.org/packages/elm/time/latest/Time#Date) по модулю `Time.Date`.

## Глибоке погруження:

Крім функції `Date.compare`, Elm також має функцію `Date.isInSameDay`, яка дозволяє перевірити, чи дві дати попадають в один день. Вона повертає `True`, якщо дати попадають в один день, і `False`, якщо ні. Ось приклад використання цієї функції:

```elm
firstDate = Date.fromString "2021-07-15"
secondDate = Date.fromString "2021-07-15"

result = Date.isInSameDay firstDate secondDate

-- результат: True
```

Також у модулі `Time.Date` є функції для отримання та зміни окремих компонентів дати, таких як день, місяць, рік тощо. Це дозволяє більш гнучко працювати з датами і виконувати різні операції з ними.

## Дивіться також:

- [Офіційна документація з модулю Time](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Порівняння дат у JavaScript, Python та PHP](https://blog.logrocket.com/comparing-dates-javascript-python-php/)
- [Розрахунок різниці між датами у Elm](https://www.smoothterminal.com/articles/calculating-differences-between-dates-in-elm)