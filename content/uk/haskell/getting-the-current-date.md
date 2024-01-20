---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:15:03.870284-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що & Навіщо?
Отримання поточної дати — це процес зчитування даних системного часу. Програмісти це роблять для логування, таймстемпів чи роботи з термінами.

## Як це зробити:
```Haskell
import Data.Time

main :: IO ()
main = do
  currentDate <- getCurrentTime
  print $ utctDay currentDate  -- Виводить поточну дату
```

Вивід прикладу:
```
2023-04-12
```

## Поглиблене вивчення:
В Haskell отримання дати почалося з модуля `Data.Time`, запровадженого в GHC з 6.6 версії. Цей модуль оновлює `System.Time` з `old-time` бібліотеки, яка наразі застаріла.

Альтернативи до `Data.Time` включають `old-time` для застарілих проектів та `time-recurrence` для роботи з інтервалами часу. Однак `Data.Time` — це стандарт де-факто через його повноту та надійність.

Щодо реалізації, `getCurrentTime` звертається до системного годинника. Воно повертає `UTCTime`, стандартний тип для часу в Haskell, що включає дату та час у форматі координованого світового часу (UTC).

## Дивіться також:
- [Data.Time library on Hackage](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Haskell Time library documentation](https://hackage.haskell.org/package/time)
- [GHC User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)