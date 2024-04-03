---
date: 2024-01-20 17:56:06.617869-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0429\u043E\u0431 \u043E\u0442\u0440\u0438\u043C\u0430\u0442\u0438 \u0430\u0440\
  \u0433\u0443\u043C\u0435\u043D\u0442\u0438 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\
  \u043E\u0433\u043E \u0440\u044F\u0434\u043A\u0430 \u0432 Haskell, \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0439\u0442\u0435 \u043C\u043E\u0434\
  \u0443\u043B\u044C `System.Environment`. \u041E\u0441\u044C \u044F\u043A \u0446\u0435\
  \ \u043F\u0440\u0430\u0446\u044E\u0454."
lastmod: '2024-03-13T22:44:49.386289-06:00'
model: gpt-4-1106-preview
summary: "\u0429\u043E\u0431 \u043E\u0442\u0440\u0438\u043C\u0430\u0442\u0438 \u0430\
  \u0440\u0433\u0443\u043C\u0435\u043D\u0442\u0438 \u043A\u043E\u043C\u0430\u043D\u0434\
  \u043D\u043E\u0433\u043E \u0440\u044F\u0434\u043A\u0430 \u0432 Haskell, \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0439\u0442\u0435 \u043C\u043E\
  \u0434\u0443\u043B\u044C `System.Environment`."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0430\u0440\u0433\u0443\u043C\u0435\
  \u043D\u0442\u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\u043E\
  \ \u0440\u044F\u0434\u043A\u0430"
weight: 23
---

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
