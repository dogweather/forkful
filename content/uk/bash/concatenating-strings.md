---
title:                "З'єднання рядків"
html_title:           "Bash: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Зачем

Конкатенування строк є важливою частиною програмування в bash. Воно дозволяє комбінувати різні значення та створювати більш складні та функціональніші рядки.

## Як це робити

```Bash 
# Приклад 1: З'єднати дві прості строки
name="John"
echo "Hello, $name!" 
# Вивід: Hello, John!

# Приклад 2: З'єднати дві змінні
message="Hello,"
name="John"
echo "$message $name!" 
# Вивід: Hello, John!

# Приклад 3: З'єднати строку та результат функції
uppercase_name=$(echo $name | tr '[:lower:]' '[:upper:]')
echo "Hello, $uppercase_name!" 
# Вивід: Hello, JOHN!
```

## Підробиці

Конкатенування строк можна розглядати як з'єднання різних частин для створення більш повноцінного рядка. У bash є кілька способів це зробити, включаючи використання подвоєних лапок для заміщення змінних та спеціальних символів, а також команди `printf`, яка дозволяє використовувати форматування. Додаткову інформацію про конкатенування строк можна знайти в [офіційній документації](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion) для bash.

## Подивіться також

- [Посібник Шелла по конкатенуванню строк](https://www.shell-tips.com/bash/string-concatenation-in-bash/)
- [Використання подвоєних лапок в bash](https://www.shell-tips.com/bash/double-quotes-vs-single-quotes-in-bash/)
- [Приклади використання команди `printf`](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)