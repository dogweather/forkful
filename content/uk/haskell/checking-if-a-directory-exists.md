---
title:                "Перевірка наявності директорії"
html_title:           "Haskell: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що це й навіщо?
Перевірка наявності директорії полягає в тому, щоб дізнатися, чи існує певна директорія в файловій системі. Програмісти роблять це, щоб уникнути помилок при спробі роботи з директорією, яка може бути неіснуючою.

## Як це робиться:
У Haskell ми можемо використовувати функцію doesDirectoryExist з модуля System.Directory для перевірки існування директорії. Ось невеликий приклад:

```Haskell
import System.Directory

main :: IO ()
main = do
    let dirName = "c:/temp"
    dirExists <- doesDirectoryExist dirName
    putStrLn $ "Does directory " ++ dirName ++ " exist? " ++ show dirExists
```

## Глибше занурення
Перевірка наявності директорії - це важлива частина роботи з файловими системами, що з'явилася на ранніх стадіях розробки комп'ютерних систем. Альтернативою може бути використання обробки помилок: просто спробувати доступ до директорії, а потім обробити помилку, якщо вона виникне. Щодо деталей реалізації, функція doesDirectoryExist працює, використовуючи системні виклики для перевірки існування директорії.

## Дивитися також
1. [Документація про System.Directory](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)
2. [Haskell Wiki про обробку файлів і директорій](https://wiki.haskell.org/Introduction_to_IO/File_IO)