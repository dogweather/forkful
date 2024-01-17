---
title:                "Читання аргументів командного рядка"
html_title:           "Fish Shell: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що та чому?
Читання аргументів з командного рядка - це процес, коли програмісти отримують дані з командного рядка під час виконання своїх програм. Це дозволяє їм створити більш гнучкі та придатні до використання програми.

## Як це зробити:
```Fish Shell``` має вбудовану функцію ```fish_parse_arguments```, яка дозволяє читати аргументи з командного рядка. Наприклад, для використання цієї функції й поточного параметру сценарію можна використовувати такий код:
```
set args (fish_parse_arguments ^ --flag)
echo "Аргументи: $args"
echo "Флаги: $args[flag]"
```
При виконанні сценарію з командою ```./script.sh param1 --flag param2``` виведе наступне:
```
Аргументи: param1 param2
Флаги: yes
```

Також, можна використовувати різні спеціальні аргументи для доступу до інформації про поточний сценарій, наприклад:
```
set argv --arg1 value1 --arg2 value2
set args (fish_parse_arguments ^ --arg1 --arg2)
echo "Значення аргументу --arg1: $args[arg1]"
echo "Значення аргументу --arg2: $args[arg2]"
```

## Глибоке занурення
Читання аргументів з командного рядка було поширеною практикою у багатьох програмувальних мовах. Наприклад, у Shell, Python, Ruby та інших мовах це можливо зробити за допомогою спеціальних функцій або модулів. Можна також реалізувати цю функціональність самостійно, використовуючи стандартні засоби програмування.

## Також дивіться
- [Документація з функції fish_parse_arguments](https://fishshell.com/docs/current/cmds/fish_parse_arguments.html)
- [Стаття про роботу з командними аргументами в Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#Bash-Variables)