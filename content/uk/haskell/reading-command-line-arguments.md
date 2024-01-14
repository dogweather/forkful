---
title:                "Haskell: Читання аргументів командного рядка."
simple_title:         "Читання аргументів командного рядка."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Чому

Командний рядок - це важливий інструмент у багатьох мовах програмування, у тому числі і в Haskell. Його основною метою є забезпечення можливості передачі вхідних даних в програму при її запуску. Це може бути корисно для взаємодії з користувачем або для обробки великих масивів даних. Тому вивчення роботи з командним рядком є важливим кроком у розвитку навичок програмування.

# Як це зробити

Для того, щоб зчитувати аргументи командного рядка в Haskell, ми можемо скористатися функцією `getArgs` з модуля `System.Environment`. Давайте подивимося на приклад коду, який виводить передані при запуску аргументи на екран:

```Haskell
import System.Environment

main = do
  args <- getArgs
  print args
```

Приклад виклику з командного рядка та виходом програми буде наступний:

```bash
runhaskell program.hs argument1 argument2
["argument1", "argument2"]
```

Використовуючи цю функцію, ми можемо зчитувати будь-яку кількість аргументів та обробляти їх вже в коді нашої програми.

# Глибокий занурення

Функція `getArgs` повертає список аргументів командного рядка у вигляді рядків. Якщо ви хочете обробити ці аргументи як числа або булеві значення, ви можете використовувати вбудовані функції для конвертації типів даних, такі як `read` або `toBool`. Також, важливо пам'ятати, що аргументи командного рядка передаються в тому ж порядку, в якому їх ввели при запуску програми.

# Дивись також

- [Haskell Wiki: Command line arguments](https://wiki.haskell.org/Command_line_arguments)
- [Hackage: System.Environment](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [Real World Haskell: Command-line argument handling](http://book.realworldhaskell.org/read/command-line.html)