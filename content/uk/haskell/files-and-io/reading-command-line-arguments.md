---
title:                "Читання аргументів командного рядка"
date:                  2024-01-20T17:56:06.617869-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що і чому?
Читання аргументів командного рядка - це процес отримання вхідних даних, які передаються вашій програмі через термінал. Програмісти роблять це для того, щоб робити програми гнучкішими і налаштовуваними без зміни коду.

## Як це зробити:
Щоб отримати аргументи командного рядка в Haskell, використовуйте модуль `System.Environment`. Ось як це працює:

```Haskell
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ show args)
```

Якщо ви запустите програму так: `runhaskell YourProgram.hs arg1 arg2`, отримаєте:

```
Hello, ["arg1","arg2"]
```

## Поглиблений розгляд
З історичного пункту зору, читання аргументів командного рядка було важливою частиною інтерфейсу командного рядка з самого початку UNIX. У Haskell, `System.Environment` є стандартними значеннями для взаємодії з середовищем ОС.

Альтернативи:
- `System.Console.GetOpt` для більш складного аналізу аргументів з флагами та опціями.
- Пакунки третіх сторін, як `optparse-applicative` для створення більш сучасних CLI додатків.

Деталі реалізації:
- `getArgs` отримує список аргументів як список рядків (`[String]`).
- `getProgName` використовується для отримання назви запущеної програми.
- Використання `getEnv` може допомогти отримати значення змінних середовища.

## Дивитись також:
- Haskell Docs: https://haskell.org/documentation
- Learn You a Haskell for Great Good (free online book): http://learnyouahaskell.com/
- "Real World Haskell" book for deeper understanding: http://book.realworldhaskell.org/
