---
title:                "Визначення довжини рядка"
date:                  2024-01-20T17:46:58.648770-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що та Чому?
Обчислення довжини рядка — це визначення кількості символів у ньому. Програмісти роблять це для валідації вводу, обробки тексту чи управління даними.

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
