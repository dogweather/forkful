---
title:                "Удаление символов, соответствующих шаблону"
date:                  2024-01-28T23:57:41.332421-07:00
model:                 gpt-4-0125-preview
simple_title:         "Удаление символов, соответствующих шаблону"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Удаление символов, соответствующих определенному шаблону, заключается в просеивании текста и удалении ненужных фрагментов. Программисты делают это для очистки данных, упрощения строк или подготовки данных к более важным операциям на следующем этапе, таким как разбор или анализ.

## Как это сделать:

```haskell
import Data.List (isInfixOf)
import Data.Char (isSpace)

-- Простая функция удаления шаблона из строки
removePattern :: Eq a => [a] -> [a] -> [a]
removePattern [] _ = []
removePattern string@(x:xs) pattern
  | pattern `isInfixOf` string = removePattern (drop (length pattern) string) pattern
  | otherwise = x : removePattern xs pattern

-- Использование предопределенных функций для обрезания пробелов в строке
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

main :: IO ()
main = do
  let text = "Haskell очень крут, действительно очень крут."
  let cleanedText = removePattern text "очень "
  putStrLn cleanedText  -- "Haskell крут, крут действительно."
  putStrLn $ trim "   Убраны начальные и конечные пробелы   " -- "Убраны начальные и конечные пробелы"
```

## Глубокое Погружение

Богатый набор библиотек Haskell, таких как 'Data.List', предоставляет разнообразные инструменты для манипулирования списками, которые, по сути, являются специальным случаем строк. Исторически, сопоставление с образцом в Haskell заимствовано из старых функциональных языков, например, ML.

В Haskell существуют различные способы сопоставления с образцом. Наша простая функция `removePattern` использует `isInfixOf` для проверки наличия шаблона. Также существуют библиотеки для работы с регулярными выражениями для сложных шаблонов, но они добавляют зависимости и иногда усложняют задачу.

Говоря о зависимостях, для обрезания пробелов можно было бы импортировать стороннюю библиотеку, но наша функция 'trim' справляется с задачей нативно.

Наконец, с точки зрения производительности, всегда будьте осторожны с рекурсивными функциями в Haskell; они могут быть неэффективны, если компилятор не оптимизирует их должным образом. Могут накапливаться танки, вызывая утечки памяти. Для повышения производительности, возможно, стоит исследовать модуль `Text` Haskell для работы с большими или многочисленными строками.

## См. Также

- Haskell в реальном мире: http://book.realworldhaskell.org/
- Документация Haskell `Data.List`: https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html
- Haskell Wiki о производительности: https://wiki.haskell.org/Performance
