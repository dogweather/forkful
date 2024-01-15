---
title:                "Читання аргументів командного рядка"
html_title:           "Python: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Вивчення обробки командного рядка є важливим навичкою для кожного програміста, оскільки це дозволяє збільшити ефективність та гнучкість вашої програми. Через користування аргументами командного рядка, ви можете змінювати поведінку вашої програми без необхідності внесення змін в сам код.

## Як використовувати

Для зчитування аргументів командного рядка використовується стандартний модуль `argparse`. Приклад коду для створення простого парсера аргументів:

```Python
import argparse 

parser = argparse.ArgumentParser(description='Приклад парсера аргументів')
parser.add_argument('number', type=int, help='Введіть число')
parser.add_argument('--double', action='store_true', help='Роздвоює введене число')

args = parser.parse_args()
number = args.number
if args.double:
    number *= 2

print(number)
```

При запуску цього коду з командного рядка `python script.py 10`, ви отримаєте виведення `10`. Якщо ж виконати командою `python script.py --double 10`, то виведення буде `20`.

## Глибоке занурення

Крім того, що ви можете зчитувати значення аргументів, ви також можете налаштовувати поведінку вашого парсера. Наприклад, ви можете задавати типи аргументів (ціле число, рядок, тощо), обов'язковість та додаткові прапори для аргументів. Для більш детальної інформації про можливості модуля `argparse`, рекомендуємо ознайомитись з [офіційною документацією](https://docs.python.org/3/library/argparse.html).

## Дивіться також

- [Офіційна документація про модуль `argparse`](https://docs.python.org/3/library/argparse.html)
- [Стаття про роботу з аргументами командного рядка в Python](https://realpython.com/command-line-interfaces-python-argparse/)