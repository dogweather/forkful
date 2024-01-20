---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що і для чого?

Відшукати довжину рядка - це значить визначити кількість символів у ньому. Програмісти виконують цю дію для обробки та маніпуляцій з даними.

## Як це зробити:

```Haskell
str_len :: String -> Int 
str_len = length
```

Наприклад, якщо ми спробуємо визначити довжину рядка "Привіт, світе!", то отримаємо п'ятнадцять:

```Haskell
main = print(str_len "Привіт, світе!")
```

В результаті отримуємо: 

```Haskell
15
```

## Детальніше

***Історичний контекст***: У літерних мовах, таких як Haskell, робота із рядками є незамінною і простою дією.

***Альтернативи***: Інша можливість - використовувати функцію `foldl'`, що створює акумулятор для рахування символів.

```Haskell
import Data.List (foldl')

str_len' :: String -> Int
str_len' = foldl' (\n _ -> n + 1) 0
```
Однак, для обчислення довжини рядка рекомендується використовувати вбудовану функцію `length`, оскільки вона простіша та зрозуміліша.

***Деталі виконання***: Функція `length` обраховує довжину рядка, працюючи з ним як зі списком символів, що забезпечує універсальність для будь-яких типів даних `String`.

## Якщо цікавить детальніше

1. [Введення в тип String](https://www.haskell.org/tutorial/strings.html)
2. [Функція length в Prelude](http://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:length)
3. [Функція foldl' в Data.List](http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:foldl-)