---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:41.074362-07:00
description: "\u041A\u0430\u043A: \u0417\u0430\u043F\u0438\u0441\u044C \u0442\u0435\
  \u043A\u0441\u0442\u043E\u0432\u044B\u0445 \u0444\u0430\u0439\u043B\u043E\u0432\
  \ \u0432 Haskell \u043F\u0440\u043E\u0441\u0442\u0430. \u0412\u043E\u0442 \u043E\
  \u0441\u043D\u043E\u0432\u043D\u043E\u0439 \u043F\u0440\u0438\u043D\u0446\u0438\u043F\
  \ \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435\u043C `writeFile`."
lastmod: '2024-03-13T22:44:45.167145-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u043F\u0438\u0441\u044C \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u044B\u0445 \u0444\u0430\u0439\u043B\u043E\u0432 \u0432 Haskell \u043F\u0440\
  \u043E\u0441\u0442\u0430."
title: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 24
---

## Как:
Запись текстовых файлов в Haskell проста. Вот основной принцип с использованием `writeFile`:

```Haskell
import System.IO

main :: IO ()
main = do
  let content = "Привет, файл!"
  writeFile "greetings.txt" content
```

Этот код создает файл `greetings.txt` с содержимым "Привет, файл!".

Для добавления текста используйте `appendFile`:

```Haskell
appendToFile :: FilePath -> String -> IO ()
appendToFile file content = appendFile file content

-- Использование
main :: IO ()
main = appendToFile "greetings.txt" "\nУвидимся скоро!"
```

Теперь в `greetings.txt` в конце также будет "Увидимся скоро!".

## Подробнее
Функции записи файлов в Haskell основаны на его надежной обработке ввода-вывода. `writeFile` и `appendFile` являются удобными оболочками для более низкоуровневых операций. Альтернативы вроде `hPutStr` или `hPutStrLn` обеспечивают больше контроля, позволяя указывать дескриптор файла.

Детали:
- `writeFile`: перед записью урезает файл.
- `appendFile`: не урезает, просто добавляет в конец.
- Обе функции обрабатывают текстовое кодирование и буферизацию.
- Для не текстовых данных используйте функции вида `hPutBuf`.

## См. также
Для получения дополнительной информации и лучших практик:

- [Документация Haskell по вводу-выводу](https://haskell.org/documentation)
- [Изучаем Haskell для хороших детей! - Ввод-вывод](http://learnyouahaskell.com/input-and-output)
- [Haskell на практике - Работа с файлами](http://book.realworldhaskell.org/read/io.html)
