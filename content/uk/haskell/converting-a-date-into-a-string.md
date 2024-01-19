---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що це і навіщо?
Конвертація дати в рядок - це процес переведення дати, представленої в цифровому форматі, в текстовий формат. Програмісти роблять це для зручності відображення дати і її наступної обробки.

## Як це зробити:
У Haskell це можна зробити за допомогою бібліотеки Data.Time та функції formatTime. Давайте покажемо приклад:
```Haskell
import Data.Time

main :: IO ()
main = do
    currentTime <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%d-%m-%Y" currentTime
    putStrLn formattedTime
```
Запускаючи цей код, ви отримаєте поточну дату у вигляді рядка у форматі "дд-мм-гггг".

## По глибше:
1. Історичний контекст: Функцію formatTime було вперше реалізовано в GHC 6.8.1 в 2007 році як частина модуля System.Locale.
2. Альтернативи: Ви також можете використовувати функцію show для конвертації UTCTime в рядок, але вона не дозволяє вам кастомізувати формат.
3. Деталі реалізації: Функція formatTime використовує типи з System.Locale та System.Time для генерації рядка з Time.

## Дивитись також:
[Офіційна документація Haskell по Data.Time](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)

Екосистема Haskell може бути складною, але сподіваємося, ця стаття допомогла вам розібратися з процесом конвертації дати в рядок!