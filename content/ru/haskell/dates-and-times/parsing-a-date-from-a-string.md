---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:15.842986-07:00
description: "\u041A\u0430\u043A \u0441\u0434\u0435\u043B\u0430\u0442\u044C: Haskell\
  \ \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\u0435\u0442 \u043D\u0435\u0441\
  \u043A\u043E\u043B\u044C\u043A\u043E \u0441\u043F\u043E\u0441\u043E\u0431\u043E\u0432\
  \ \u0440\u0430\u0437\u0431\u043E\u0440\u0430 \u0434\u0430\u0442, \u043D\u043E \u0434\
  \u0430\u0432\u0430\u0439\u0442\u0435 \u0441\u043E\u0441\u0440\u0435\u0434\u043E\u0442\
  \u043E\u0447\u0438\u043C\u0441\u044F \u043D\u0430 \u0431\u0438\u0431\u043B\u0438\
  \u043E\u0442\u0435\u043A\u0435 `time` \u0438 \u043D\u0430 \u043F\u0440\u043E\u0441\
  \u0442\u043E\u043C \u043F\u0440\u0438\u043C\u0435\u0440\u0435 \u0441 \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C\u2026"
lastmod: '2024-03-13T22:44:45.151118-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\u0435\u0442 \u043D\
  \u0435\u0441\u043A\u043E\u043B\u044C\u043A\u043E \u0441\u043F\u043E\u0441\u043E\u0431\
  \u043E\u0432 \u0440\u0430\u0437\u0431\u043E\u0440\u0430 \u0434\u0430\u0442, \u043D\
  \u043E \u0434\u0430\u0432\u0430\u0439\u0442\u0435 \u0441\u043E\u0441\u0440\u0435\
  \u0434\u043E\u0442\u043E\u0447\u0438\u043C\u0441\u044F \u043D\u0430 \u0431\u0438\
  \u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0435 `time` \u0438 \u043D\u0430 \u043F\
  \u0440\u043E\u0441\u0442\u043E\u043C \u043F\u0440\u0438\u043C\u0435\u0440\u0435\
  \ \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435\u043C `parseTimeM`."
title: "\u0410\u043D\u0430\u043B\u0438\u0437 \u0434\u0430\u0442\u044B \u0438\u0437\
  \ \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 30
---

## Как сделать:
Haskell предлагает несколько способов разбора дат, но давайте сосредоточимся на библиотеке `time` и на простом примере с использованием `parseTimeM`. Убедитесь, что у вас установлен пакет `time`.

```haskell
import Data.Time.Format (parseTimeM, defaultTimeLocale)

main :: IO ()
main = do
  let dateString = "2023-03-21"
  let parsedDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString :: IO (Maybe Day)
  
  result <- parsedDate
  case result of
    Just day -> putStrLn $ "Parsed date: " ++ show day
    Nothing -> putStrLn "Failed to parse date."

-- Вывод должен быть: Parsed date: 2023-03-21
```

## Подробнее
Исторически разбор дат обрабатывался по-разному в разных языках и библиотеках, многие из которых использовали вариации на тему шаблонов `strftime` из языка C. Библиотека `time` в Haskell следует этому подходу для обеспечения согласованности. Альтернативы `time` включают использование пакета `old-time`, который теперь считается устаревшим, или сторонних библиотек, таких как `thyme` или `chronos`.

С точки зрения реализации, разбор в Haskell является типобезопасным, отсюда использование `Maybe` в примере для обработки ошибок разбора. Функция `parseTimeM` использует вывод типов для определения возвращаемого типа, что делает ее гибкой. Понимание спецификаторов формата, таких как `%Y-%m-%d` для года-месяца-дня, имеет решающее значение.

 Сильная система типов в Haskell гарантирует, что как только дата разобрана, абсолютно ясно и однозначно, какого она типа, что снижает количество ошибок выполнения, связанных с манипуляцией с датами. Однако эта строгость требует обработки случаев, когда входные данные не соответствуют ожидаемому шаблону, отсюда сопоставление с образцом `Just` и `Nothing`.

## Смотрите также
- Документация библиотеки Haskell `time`: [https://hackage.haskell.org/package/time](https://hackage.haskell.org/package/time)
- Руководство "Learn You a Haskell" о датах и времени: [http://learnyouahaskell.com/](http://learnyouahaskell.com/) - (искать раздел "Data.Time")
- Спецификаторы формата для `Data.Time.Format`: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime)
