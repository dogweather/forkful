---
title:                "Читання аргументів командного рядка"
html_title:           "Haskell: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Запозичення командного рядка дозволяє зчитувати зовнішні дані та покращити взаємодію з користувачем. Це особливо корисно для інтерактивних програм та інструментів розробників.

## Як

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn "Введіть довільну фразу:"
    input <- getLine
    putStrLn ("Ваша фраза: " ++ input)
    putStrLn ("Перша аргумент командного рядка: " ++ (args !! 0))
```

**Вхід:** `Hello Haskell!`

**Вихід:** 
```
Введіть довільну фразу:
Hello Haskell!
Ваша фраза: Hello Haskell!
Перша аргумент командного рядка: Hello
```
У цьому прикладі ми використовуємо модуль `System.Environment`, щоб отримати аргументи командного рядка та користувацький ввід. Значення аргументів зберігаються у списку `args`, який можна обробити за допомогою функцій маніпулювання списками. Користувач може ввести будь-яку фразу, яка виводиться наступним рядком. 

## Глибоке погруження

Крім отримання аргументів командного рядка та користувацького вводу, є й інші способи взаємодії з командним рядком. Наприклад, функція `getProgName` повертає назву програми, яка виконується, а `getEnvironment` дозволяє доступ до змінних середовища.

Якщо ви бажаєте отримати більш детальну інформацію про роботу з аргументами командного рядка в Haskell, радимо ознайомитися з документацією на сайті [Haskell.org](https://www.haskell.org/documentation/).

## Дивись також

- [Документація з модулю `System.Environment`](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html)
- [Розбір аргументів командного рядка в Haskell](https://dev.to/skyrkling/command-line-args-in-haskell-1ik3)