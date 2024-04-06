---
date: 2024-01-20 17:54:41.603932-07:00
description: "How to: (\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) \u041F\u0456\
  \u0434 \u0447\u0430\u0441 \u0437\u0430\u043F\u0443\u0441\u043A\u0443 `main`, \u043F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0430 \u0432\u0438\u0432\u0435\u0434\u0435\
  \ \u0432\u043C\u0456\u0441\u0442 \u0444\u0430\u0439\u043B\u0443 `test.txt`."
lastmod: '2024-04-05T21:53:49.561198-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) \u041F\u0456\u0434\
  \ \u0447\u0430\u0441 \u0437\u0430\u043F\u0443\u0441\u043A\u0443 `main`, \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0430 \u0432\u0438\u0432\u0435\u0434\u0435 \u0432\
  \u043C\u0456\u0441\u0442 \u0444\u0430\u0439\u043B\u0443 `test.txt`."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

## How to: (Як робити:)
```Haskell
-- Імпортуємо модуль
import System.IO

-- Функція для читання файлу
readFileExample :: FilePath -> IO ()
readFileExample filePath = do
    content <- readFile filePath
    putStrLn "Содержимое файла:"
    putStrLn content

main :: IO ()
main = readFileExample "test.txt"
```
Під час запуску `main`, програма виведе вміст файлу `test.txt`.

## Deep Dive (Поглиблене вивчення)
Історично, читання файлів в Haskell використовувало `lazy IO`, але з часом програмісти стикались з проблемами з управлінням ресурсів через непередбачуваний порядок виконання. Сучасні бібліотеки, як-от `text` або `bytestring`, пропонують більше контролю через `strict IO`. Вибір `lazy` чи `strict` версії залежить від конкретної задачі. Розуміння монад IO у Haskell також критично важливо для правильної роботи з файлами.

Альтернативи `readFile` включають `readFile'` з бібліотеки `text`, яка прочитає вміст файлу строго, а `Data.ByteString` пропонує аналогічну функціональність для роботи з байтами.

## See Also (Дивіться також)
- [Haskell Wiki on IO](https://wiki.haskell.org/IO_inside)
- [Text library](https://hackage.haskell.org/package/text)
- [Bytestring library](https://hackage.haskell.org/package/bytestring)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/input-and-output) - введення в IO у Haskell.
