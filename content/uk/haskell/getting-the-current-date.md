---
title:    "Haskell: Отримання поточної дати."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Чому

Отримання поточної дати є важливою та корисною функцією при програмуванні. Воно дозволяє нам отримувати точний час та дату, що може бути корисно при роботі зі змінними, їх порівнянням та сортуванням.

##Як

Загальним способом отримання поточної дати в Haskell є використання зовнішньої бібліотеки *time*. Спочатку необхідно встановити цю бібліотеку за допомогою команди `cabal install time`. Після цього, для отримання поточної дати, можна використати функцію *getCurrentTime*, яка повертає значення типу *IO UTCTime*.

```Haskell
import Data.Time

main = do
    currentTime <- getCurrentTime
    print currentTime
```

В результаті виконання програми ми отримаємо поточну дату та час у форматі *UTCTime*. Це значення можна перетворити в більш зрозумілий формат за допомогою функції *utcToLocalTime*, яка приймає в якості аргументу часовий пояс.

```Haskell
import Data.Time

main = do
    currentTime <- getCurrentTime
    let localTime = utcToLocalTime (hoursToTimeZone 2) currentTime
    print localTime
```

У даному прикладі ми перетворили часовий пояс UTC +02:00 в локальний час, та отримали значення типу *LocalTime*, яке може бути використане для подальших маніпуляцій з датами.

##Глибокий розбір

Отримання поточної дати відрізняється у Haskell в порівнянні з іншими мовами програмування. Одною з головних особливостей цього процесу є використання типу *UTCTime*, що використовується для зберігання дати та часу в універсальному часовому поясі UTC. Це дозволяє уникнути проблем з часовими поясами та конфліктами з переходом на літній та зимовий час. Також, використання типу *UTCTime* дає можливість точно визначити часову зону, в якій поточна дата та час були отримані.

See Also
- `Data.Time` документація - https://hackage.haskell.org/package/time/docs/Data-Time.html
- Універсальний часовий пояс (UTC) - https://uk.wikipedia.org/wiki/%D0%A3%D0%BD%D1%96%D0%B2%D0%B5%D1%80%D1%81%D0%B0%D0%BB%D1%8C%D0%BD%D0%B8%D0%B9_%D1%87%D0%B0%D1%81%D0%BE%D0%B2%D0%B8%D0%B9_%D0%BF%D0%BE%D1%8F%D1%81
- Серія уроків по роботі з датами в Haskell - https://mmhaskell.com/blog/2017/5/22/the-ultimate-guide-to-datetimes-in