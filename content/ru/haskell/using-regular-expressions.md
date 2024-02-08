---
title:                "Использование регулярных выражений"
aliases:
- ru/haskell/using-regular-expressions.md
date:                  2024-01-29T00:03:32.846793-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?
Регулярные выражения (regex) ищут и манипулируют строками, основываясь на шаблонах. Программисты используют их для задач, таких как проверка форм, анализ данных и обработка текста, поскольку они мощные и лаконичные.

## Как использовать:
В Haskell регулярные выражения можно использовать с пакетом `regex-tdfa`. Здесь мы извлекаем числа из строки.

```Haskell
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  let text = "Order 531 has 2 items"
  let numbers = text =~ "[0-9]+" :: [String]
  print numbers
```

Вывод:
```
["531","2"]
```

Чтобы заменить текст, можно использовать `subRegex` из `regex-compat`.

```Haskell
import Text.Regex (subRegex, mkRegex)

main :: IO ()
main = do
  let text = "Hello, 2023!"
  let regex = mkRegex "[0-9]+"
  let newText = subRegex regex text "YEAR"
  putStrLn newText
```

Вывод:
```
Hello, YEAR!
```

## Подробнее
Регулярные выражения восходят к 1950-м годам, они были концептуализированы математиком Стивеном Клини. Хотя Haskell пришел к этому позднее, сейчас в нем существует богатый набор библиотек regex, таких как `regex-tdfa` для POSIX regex и `regex-pcre` для совместимости с Perl. Альтернативы regex включают в себя библиотеки парсер-комбинаторов, такие как `parsec`, которые могут предложить большую читаемость и обслуживаемость. Регулярные выражения в Haskell не встроены в синтаксис языка напрямую, но предоставляются через эти библиотеки.

## Смотрите также
- Библиотеки Hackage:
  - regex-tdfa: http://hackage.haskell.org/package/regex-tdfa
  - regex-compat: http://hackage.haskell.org/package/regex-compat
  - regex-pcre: http://hackage.haskell.org/package/regex-pcre
- Вики Haskell по регулярным выражениям: https://wiki.haskell.org/Regular_expressions
- "Real World Haskell" Брайана О'Салливана, Дона Стюарта и Джона Гоерзена для глубокого изучения: http://book.realworldhaskell.org/
