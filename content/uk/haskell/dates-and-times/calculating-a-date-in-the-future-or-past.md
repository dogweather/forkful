---
date: 2024-01-20 17:31:42.768859-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0413\u0430\u0440\u043D\u043E, \u043D\u0430\u043F\u0438\u0448\u0435\u043C\u043E\
  \ \u0444\u0443\u043D\u043A\u0446\u0456\u044E. \u0412\u0430\u0441 \u0437\u043D\u0430\
  \u0439\u043E\u043C\u0438\u0442\u0438 \u0437 `Data.Time` \u0431\u0456\u0431\u043B\
  \u0456\u043E\u0442\u0435\u043A\u043E\u044E."
lastmod: '2024-03-13T22:44:49.382758-06:00'
model: gpt-4-1106-preview
summary: "\u0413\u0430\u0440\u043D\u043E, \u043D\u0430\u043F\u0438\u0448\u0435\u043C\
  \u043E \u0444\u0443\u043D\u043A\u0446\u0456\u044E."
title: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443"
weight: 26
---

## Як це зробити:
Гарно, напишемо функцію. Вас знайомити з `Data.Time` бібліотекою.

```Haskell
import Data.Time

addDaysToCurrent :: Integer -> IO Day
addDaysToCurrent n = do
  currentDay <- utctDay <$> getCurrentTime
  return $ addDays n currentDay

main :: IO ()
main = do
  putStrLn "Сьогодні плюс 10 днів буде:"
  tenDaysLater <- addDaysToCurrent 10
  print tenDaysLater
```

Запустимо код і отримаємо:
```
Сьогодні плюс 10 днів буде:
2023-04-21
```

## Поглиблений розгляд:
Обчислення дати було завжди актуальним. Астрономи, історики й банкіри — всі працювали з календарями. В Haskell, `Data.Time` це стандартна бібліотека для роботи з часом. Вона охоплює часові зони, і на відміну від старої `Data.Time.Calendar`, враховує переведення часу. Альтернативи — `time` і `old-time`, але `Data.Time` використовувати простіше і воно має повнішу підтримку таймзон.

## Дивіться також:
- [Haskell Data.Time Library](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Learn You a Haskell for Great Good! (by Miran Lipovača)](http://learnyouahaskell.com/) - Легкий вступ до Haskell.

Тримайтеся просто, глибоко розумійте свої інструменти, і програмуйте щасливо!
