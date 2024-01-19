---
title:                "Читання аргументів командного рядка"
html_title:           "Arduino: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Python: Як читати аргументи командного рядка

## Що і навіщо?

Аргументи командного рядка - це параметри, які передаємо нашій програмі при її запуску. Це корисно, дозволяючи нам контролювати програму без змін в коді.

## Як це зробити:

Модуль `sys` у Python містить список `argv`, що містить аргументи командного рядка. Перший аргумент ( `argv [0]` ) - це назва нашого скрипта Python.

```Python
import sys

print("Назва скрипта:", sys.argv[0])

# якщо аргументи передані
if len(sys.argv) > 1:
    print("Аргументи:", sys.argv[1:])
```

Коли виконаємо скрипт з аргументами, отримаємо наступне:

```shell
$ python3 test.py arg1 arg2 arg3
Назва скрипта: test.py
Аргументи: ['arg1', 'arg2', 'arg3']
```

## Поглиблений огляд:

1. Історичний контекст: Читання аргументів командного рядка використовується основними мовами програмування ще з часів С і Бейсик.
2. Альтернативи: Для більш складних випадків, таких як обробка опцій CLI, бібліотеки, наприклад `argparse`, можуть бути дуже корисними.
3. Деталі реалізації: Модуль `sys` Python заповнює список `argv` з аргументів командного рядка, коли інтерпретатор Python стартує. 

## Див. також:

- Документація по Python на `sys.argv`: https://docs.python.org/uk/3/library/sys.html#sys.argv
- Посібник по модулю Python `argparse`: https://docs.python.org/uk/3/library/argparse.html#module-argparse