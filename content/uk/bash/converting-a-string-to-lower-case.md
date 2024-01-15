---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Bash: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Існує безліч ситуацій, коли нам потрібно перетворити рядок на нижній регістр у нашому Bash скрипті. Наприклад, в ході обробки вводу від користувача або порівняння двох рядків.

## Як це зробити

Ось простий спосіб перетворення рядка на нижній регістр за допомогою вбудованої команди `tr`:

```bash
# Задамо змінній `my_string` значення 'Hello, World!'
my_string='Hello, World!'

# Виведемо результат на екран
echo "$my_string" | tr '[:upper:]' '[:lower:]'
```

В результаті ми отримаємо `hello, world!`.

## Глибше

Існує кілька способів перетворення рядка на нижній регістр в Bash. Один з найпростіших - за допомогою команди `tr`, яку ми побачили в прикладі вище. Ця команда змінює кожен символ у рядку на його нижній регістр. Також можна використовувати команду `sed`, використовуючи регулярні вирази, або просто вбудовану функцію `tolower()`.

## Дивись також

- [Це корисний посібник по використанню команди `tr` в Bash](https://linux.die.net/man/1/tr)
- [Документація по `sed` та регулярним виразам](https://www.gnu.org/software/sed/manual/sed.html)
- [Документація по вбудованим функціям Bash](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Builtin-Commands)