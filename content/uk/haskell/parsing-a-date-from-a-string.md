---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:37:08.224833-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що та Чому?
Розбір дати з рядка — це процес отримання структурованої інформації про дату з текстового формату. Програмісти роблять це, щоб користувачі могли вводити дати як простий текст, а потім обробляти ці дані машинами.

## Як це зробити:
Найпростіший спосіб розпізнати дату в Haskell — використовувати пакунок `time`. Розберемо на прикладі:

```Haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

-- Функція для парсингу дати з рядка
parseDate :: String -> Maybe UTCTime
parseDate dateString = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString

main :: IO ()
main = do
    let dateStr = "2023-03-14"
    case parseDate dateStr of
        Just date -> putStrLn $ "Успішно розпізнано дату: " ++ show date
        Nothing -> putStrLn "Не вдалося розпізнати дату."
```

При запуску цього коду ви отримаєте:
```
Успішно розпізнано дату: 2023-03-14 00:00:00 UTC
```

## Розбір Деталей
Функція `parseTimeM` в Haskell використовується для парсингу рядків у дати. Вона належить до бібліотеки `time`, яка є стандартною для роботи з часом і датами.

Історично, робота з датами була досить складною через різноманіття форматів та календарів. В Haskell, `time` стандартизує обробку часу. Однак, є альтернативи, як-от `Data.Time.Calendar` для більш складних завдань.

Метод `%Y-%m-%d` у `formatTime` вказує на конкретний формат дати: рік, місяць, день. Змініть цей шаблон, щоб розпізнати різні формати дат.

Парсинг дати можна кастомізувати, використовуючи власні формати часу і шаблони. Для більшої гнучкості та узгодження з іншими системами можна використовувати бібліотеки, такі як `time-locale-compat`, яка забезпечує більше локалей.

## Також Гляньте
- Офіційні документи для `time` пакунка: http://hackage.haskell.org/package/time
- Про `Data.Time.Clock`: http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html
- Про `Data.Time.Format` та `formatTime`: http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html
- Про `System.Locale` та локалі: http://hackage.haskell.org/package/time-1.9.3/docs/System-Locale.html
- Про `time-locale-compat` для компатибільності локалей: http://hackage.haskell.org/package/time-locale-compat