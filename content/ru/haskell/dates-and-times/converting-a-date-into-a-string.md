---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:52.739459-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Haskell \u0432\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u0435\u0442\u0435 \u0444\u0443\u043D\u043A\u0446\u0438\u044E `formatTime`\
  \ \u0438\u0437 \u043C\u043E\u0434\u0443\u043B\u044F `Data.Time.Format` \u0434\u043B\
  \u044F \u044D\u0442\u043E\u0439 \u0437\u0430\u0434\u0430\u0447\u0438. \u0414\u0430\
  \u0432\u0430\u0439\u0442\u0435 \u0441\u0440\u0430\u0437\u0443 \u043F\u0435\u0440\
  \u0435\u0439\u0434\u0435\u043C \u043A \u043A\u043E\u0434\u0443."
lastmod: '2024-03-13T22:44:45.154668-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Haskell \u0432\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0435\u0442\u0435 \u0444\u0443\u043D\u043A\u0446\u0438\u044E `formatTime` \u0438\
  \u0437 \u043C\u043E\u0434\u0443\u043B\u044F `Data.Time.Format` \u0434\u043B\u044F\
  \ \u044D\u0442\u043E\u0439 \u0437\u0430\u0434\u0430\u0447\u0438."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0434\u0430\u0442\u044B \u0432 \u0441\u0442\u0440\u043E\u043A\u0443"
weight: 28
---

## Как это сделать:
В Haskell вы используете функцию `formatTime` из модуля `Data.Time.Format` для этой задачи. Давайте сразу перейдем к коду:

```haskell
import Data.Time
import Data.Time.Format (formatTime, defaultTimeLocale)

main :: IO ()
main = do
    -- Получение текущего времени
    currentTime <- getCurrentTime
    let currentZone = utc
        -- Преобразование времени UTC в объект местного времени
        localTime = utcToLocalTime currentZone currentTime
        -- Форматирование даты как "ГГГГ-ММ-ДД"
        dateString = formatTime defaultTimeLocale "%F" localTime
    putStrLn dateString
```

И вот что вы можете увидеть на выходе, в зависимости от текущей даты:

```
2023-04-01
```

## Подробный анализ
С самых ранних дней программирования, преобразование дат в строки всегда было вопросом практической удобности. В Haskell наше обращение с датой и временем обязано библиотеке `Data.Time`, которая была вдохновлена функциональностью и улучшениями старых библиотек, таких как `old-time`.

Существуют альтернативы `formatTime`, такие как использование `show` для прямого преобразования даты в строку, но это не даст вам опций пользовательского форматирования. Функция `formatTime` богата, поддерживая разнообразие форматов, которые соответствуют шаблонам функции `strftime` в C. Это гибко и учитывает локаль, используя `defaultTimeLocale` или другие локали для форматирования дат согласно культурным конвенциям.

В плане реализации функции `Data.Time.Format` являются чистыми, что означает, что они не зависят от побочных эффектов и не вызывают их. Это соответствует этическим принципам функционального программирования Haskell, которые стремятся к тому, чтобы функции были предсказуемыми и их результаты определялись только их входными данными.

## Смотрите также
Для более обширной работы с датами и временем в Haskell ознакомьтесь с следующим:

- Документация модуля `Data.Time`: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- Детали о строках формата `strftime`, которые имитирует `formatTime`: [http://man7.org/linux/man-pages/man3/strftime.3.html](http://man7.org/linux/man-pages/man3/strftime.3.html)
- Подход Haskell к вводу-выводу и чистоте: [https://www.haskell.org/tutorial/io.html](https://www.haskell.org/tutorial/io.html)
