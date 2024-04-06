---
date: 2024-01-20 17:46:58.648770-07:00
description: "\u042F\u043A \u0441\u0430\u043C\u0435: Historically, the `${#string}`\
  \ syntax comes from the POSIX standard for shell scripting, making it a reliable\
  \ method across different systems. As\u2026"
lastmod: '2024-04-05T22:51:02.592471-06:00'
model: gpt-4-1106-preview
summary: Historically, the `${#string}` syntax comes from the POSIX standard for shell
  scripting, making it a reliable method across different systems.
title: "\u0412\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u043E\u0432\
  \u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430"
weight: 7
---

## Як саме:
```Bash
# Використовуйте вбудовану змінну ${#string}
str="Привіт, світ!"
echo ${#str}  # Виводить довжину рядка
```
Вихідний код:
```
13
```
```Bash
# Для більш складних сценаріїв можна використовувати команду 'awk'
echo "Привіт, світ!" | awk '{print length}'
```
Вихідний код:
```
13
```

## Поглиблене занурення
Historically, the `${#string}` syntax comes from the POSIX standard for shell scripting, making it a reliable method across different systems. As for alternatives, some use `awk` with its built-in `length` function, which can be helpful in complex text processing tasks. Implementation details are simple: `${#string}` counts the characters until it reaches the end of the string, while `awk` processes the string passed to it and computes the length.

## Дивіться також
- Bash Manual for String Operations: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion
- AWK User's Guide: https://www.gnu.org/software/gawk/manual/gawk.html
- POSIX Specification: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html
