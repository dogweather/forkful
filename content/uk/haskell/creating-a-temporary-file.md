---
title:                "Створення тимчасового файлу"
html_title:           "Haskell: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Тимчасові файли є корисним інструментом в програмуванні з Haskell. Вони дозволяють зберігати тимчасові дані протягом виконання програми, що допомагає уникнути засмічення пам'яті та підвищує ефективність програми.

## Як створити тимчасовий файл

Для створення тимчасового файлу використовується функція `withSystemTempFile` з модуля `System.IO.Temp`. Ця функція приймає як аргумент замикання (closure) для обробки тимчасового файлу, яке потрібно виконати і повертає значення, яке повертає це замикання. Ось приклад використання цієї функції для створення та запису даних у тимчасовий файл:

```Haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, hGetContents, hClose)

main = do
  withSystemTempFile "temp.txt" $ \path handle -> do
    hPutStrLn handle "Тимчасові дані"
    hClose handle
    contents <- readFile path
    putStrLn ("Вміст тимчасового файлу: " ++ contents)
```
**Вихід:**
``
Вміст тимчасового файлу: Тимчасові дані
``

## Глибоке вивчення

Функція `withSystemTempFile` створює тимчасовий файл у системній тимчасовій директорії та виконує передане замикання, передаючи йому шлях тимчасового файлу та дескриптор. Після того, як замикання завершиться, файл буде автоматично видалений. Можна також використовувати функцію `withTempFile` для створення тимчасового файлу в директорії, яку ви вказали.

## Дивіться також

- [Haskell документація - Модуль `System.IO.Temp`](https://hackage.haskell.org/package/temporary/docs/System-IO-Temp.html)
- [Стаття на сайті Haskell.org - Тимчасові файли в Haskell](https://wiki.haskell.org/How_to_create_a_temporary_file)