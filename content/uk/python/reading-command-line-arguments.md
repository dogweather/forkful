---
title:    "Python: Отримання аргументів командного рядка"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Чому
Командні аргументи є важливою частиною програмування на Python. Вони дозволяють користувачам передавати параметри у вашу програму під час її запуску. Це дає більшу гнучкість та можливість взаємодії з програмою.

## Як це зробити
Для того, щоб прочитати командні аргументи у своїй програмі, вам потрібно використовувати модуль `sys` та його функцію `argv`. Приклад коду та результату:

```Python
import sys

# Читаємо командні аргументи та виводимо їх у консоль
print("Назва файла:", sys.argv[0])

# Перші аргументи після назви файлу будуть збережені у списку
arguments = sys.argv[1:]
print("Кількість аргументів:", len(arguments))

# Виводимо кожен аргумент окремо
for arg in arguments:
    print("Аргумент:", arg)
```

При запуску програми з командними аргументами, наприклад `python program.py argument1 argument2`, результат буде виглядати так:

```
Назва файла: program.py
Кількість аргументів: 2
Аргумент: argument1
Аргумент: argument2
```

## Глибоке дослідження
Крім перерахованого прикладу, існує багато інших методів для роботи з командними аргументами. Наприклад, ви можете використовувати бібліотеки `argparse` або `click`, які надають зручний інтерфейс для обробки аргументів та створення додаткових опцій для вашої програми. Також можливо додавати документацію до вашої програми для зручного використання командних аргументів.

## Дивись також
- [Офіційна документація Python про модуль `sys`](https://docs.python.org/3/library/sys.html)
- [Підручник з використання командних аргументів в Python](https://realpython.com/python-command-line-arguments/)
- [Аналогічна стаття українською мовою](https://codeburst.io/розуміння-вашого-додатку-python-із-командного-рядка-102c8db99ceb)