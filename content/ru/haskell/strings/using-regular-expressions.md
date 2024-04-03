---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:32.846793-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: \u0412 Haskell \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\
  \u044B\u0435 \u0432\u044B\u0440\u0430\u0436\u0435\u043D\u0438\u044F \u043C\u043E\
  \u0436\u043D\u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\
  \u044C \u0441 \u043F\u0430\u043A\u0435\u0442\u043E\u043C `regex-tdfa`. \u0417\u0434\
  \u0435\u0441\u044C \u043C\u044B \u0438\u0437\u0432\u043B\u0435\u043A\u0430\u0435\
  \u043C \u0447\u0438\u0441\u043B\u0430 \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\
  \u0438."
lastmod: '2024-03-13T22:44:45.115255-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Haskell \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0435\
  \ \u0432\u044B\u0440\u0430\u0436\u0435\u043D\u0438\u044F \u043C\u043E\u0436\u043D\
  \u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  \ \u0441 \u043F\u0430\u043A\u0435\u0442\u043E\u043C `regex-tdfa`."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\
  \u0430\u0436\u0435\u043D\u0438\u0439"
weight: 11
---

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
