---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо це потрібно?

Знаходження довжини рядка - це визначення кількості символів у даному рядку. Програмісти роблять це, щоб керувати обробкою даних, оптимізувати виконання коду та перевіряти правильність вхідних даних.

## Як це робити:

```Bash
# Задаємо рядок
str="Привіт, це тестовий рядок!"

# Знаходимо його довжину
len=${#str}

# Виводимо довжину рядку
echo $len
```

Виходячи з цього коду, вихідні дані будуть наступними:

```Bash
27
```

## Поглиблений матеріал:

Історично Bash використовувався для автоматизації командного рядка, але із часом почав використовуватися для багатьох інших задач. Засоби Bash для роботи з рядками в багатьох випадках були єдиним варіантом виконання задачі.

Єдиний альтернативний спосіб дізнатися довжину рядка - використати команду `expr` з параметром `length`. Однак цей підхід може бути повільним у великих системах.

```Bash
# На вхід подається рядок
str="Привіт, це тестовий рядок!" 

# Використання 'expr' для визначення довжини
len=$(expr length "$str") 

# Виводимо значення довжини
echo $len
```

## Додаткові матеріали:

Для глибшого розуміння, ви можете дивитися наступні джерела:

1. [Bash String Manipulation Guide](https://www.baeldung.com/linux/bash-string-manipulation)
2. [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/index.html)
3. [Introduction to Bash Scripting](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)