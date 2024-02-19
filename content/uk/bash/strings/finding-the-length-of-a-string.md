---
aliases:
- /uk/bash/finding-the-length-of-a-string/
date: 2024-01-20 17:46:58.648770-07:00
description: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u043E\
  \u0432\u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430 \u2014 \u0446\u0435\
  \ \u0432\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u043A\u0456\u043B\
  \u044C\u043A\u043E\u0441\u0442\u0456 \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432\
  \ \u0443 \u043D\u044C\u043E\u043C\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435\
  \ \u0434\u043B\u044F \u0432\u0430\u043B\u0456\u0434\u0430\u0446\u0456\u0457 \u0432\
  \u0432\u043E\u0434\u0443, \u043E\u0431\u0440\u043E\u0431\u043A\u0438 \u0442\u0435\
  \u043A\u0441\u0442\u0443 \u0447\u0438 \u0443\u043F\u0440\u0430\u0432\u043B\u0456\
  \u043D\u043D\u044F \u0434\u0430\u043D\u0438\u043C\u0438."
lastmod: 2024-02-18 23:09:00.629005
model: gpt-4-1106-preview
summary: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u043E\
  \u0432\u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430 \u2014 \u0446\u0435\
  \ \u0432\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u043A\u0456\u043B\
  \u044C\u043A\u043E\u0441\u0442\u0456 \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432\
  \ \u0443 \u043D\u044C\u043E\u043C\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435\
  \ \u0434\u043B\u044F \u0432\u0430\u043B\u0456\u0434\u0430\u0446\u0456\u0457 \u0432\
  \u0432\u043E\u0434\u0443, \u043E\u0431\u0440\u043E\u0431\u043A\u0438 \u0442\u0435\
  \u043A\u0441\u0442\u0443 \u0447\u0438 \u0443\u043F\u0440\u0430\u0432\u043B\u0456\
  \u043D\u043D\u044F \u0434\u0430\u043D\u0438\u043C\u0438."
title: "\u0412\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u043E\u0432\
  \u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430"
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
