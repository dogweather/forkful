---
title:                "Haskell: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Одним з найпоширеніших завдань в програмуванні є робота з датами і часом. Наприклад, ви можете створювати програми, які відстежують терміни дій, або просто відображають поточну дату у своєму інтерфейсі. У Haskell є простий спосіб отримати поточну дату, тож давайте розберемося, як це зробити.

## Як це зробити

Для отримання поточної дати у Haskell ми будемо використовувати функцію `getCurrentTime` з модуля `Data.Time`. Ця функція повертає об'єкт типу `UTCTime`, який містить інформацію про поточну дату і час. Давайте створимо простий код, який отримає поточну дату і виведе її у форматі "День, Місяць Рік":

```Haskell
import Data.Time

main = do
  time <- getCurrentTime
  let date = utctDay time
  putStrLn $ formatTime defaultTimeLocale "%A, %B %Y" date
```

Якщо запустити цей код, то ви побачите поточну дату у форматі "Понеділок, Вересень 2021". Але можна використовувати більше різних форматів, які описані у документації до модуля `Data.Time`. Наприклад, щоб повернути лише номер поточного місяця, можна використати формат "%m":

```Haskell
putStrLn $ formatTime defaultTimeLocale "%m" date
```

Це виведе поточний місяць у двоцифровому форматі, наприклад "09".

## Глибокий погляд

Якщо ви хочете розібратися за додаткову інформацію про роботу з датами у Haskell, рекомендую ознайомитися з документацією до модулів `Data.Time` і `Data.Time.Clock`. Ви можете знайти багато корисної інформації про формати дат, розрахунки часу і багато іншого.

## Дивіться також

- [Офіційна документація до модуля Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Туторіал по роботі з датами в Haskell](https://www.fpcomplete.com/blog/2017/10/working-with-dates-in-haskell/)