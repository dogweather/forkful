---
title:                "Анализ даты из строки"
aliases: - /ru/haskell/parsing-a-date-from-a-string.md
date:                  2024-01-29T00:00:15.842986-07:00
model:                 gpt-4-0125-preview
simple_title:         "Анализ даты из строки"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Разбор даты из строки означает преобразование текста в тип данных даты. Программистам часто нужно конвертировать пользовательский ввод или содержимое текстовых файлов в структурированные даты для обработки и манипуляции.

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
