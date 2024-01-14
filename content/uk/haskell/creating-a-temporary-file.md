---
title:                "Haskell: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## По тому, чому - Чому

Створення тимчасових файлів є необхідним для багатьох задач програмування. Це дозволяє зберігати тимчасові дані, які будуть використовуватися пізніше в коді. Також це допомагає уникнути переповнення пам'яті та підтримати чистоту в коді.

## Як це робити

Створення тимчасових файлів в Haskell дуже просте. Для цього використовується функція `withSystemTempFile` з модуля `System.IO.Temp`. Приклад коду виглядатиме так:

```Haskell
import System.IO.Temp (withSystemTempFile)

main :: IO ()
main = withSystemTempFile "mytempfile.txt" $ \tempFilePath tempHandle -> do
    putStrLn $ "Створений тимчасовий файл " ++ tempFilePath
    hPutStrLn tempHandle "Це мій тимчасовий файл"
```

У цьому прикладі ми використовуємо параметр `mytempfile.txt` для назви нашого тимчасового файлу. Функція `withSystemTempFile` приймає два аргументи: шлях до файлу та функцію, яка буде обробляти цей файл. У нашому випадку, ми просто виводимо інформацію про створення файлу та додаємо строку "Це мій тимчасовий файл" у файл.

Після виконання цього коду, ми отримаємо такий вихід у терміналі:

```
Створений тимчасовий файл mytempfile.txt
```

## Глибше погляньмо

Можна помітити, що функція `withSystemTempFile` використовує модуль `System.IO.Temp`. Цей модуль містить також інші корисні функції, наприклад `withTempDirectory`, яка дозволяє створити тимчасову папку замість файлу. Також варто зазначити, що ці функції автоматично видаляють створені тимчасові файли та папки після їх використання.

## Дивіться також

- [Haskell документація про створення тимчасових файлів](https://hackage.haskell.org/package/temporary)
- [Розділ про тимчасові файли у книзі "Real World Haskell"](http://book.realworldhaskell.org/read/programming-with-text.html#id641377)