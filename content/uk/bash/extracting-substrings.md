---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що І Навіщо?
Виділення підрядків - це процес витягування специфічних частин рядка. Програмісти це роблять, щоб працювати з конкретними даними в більшому обсязі інформації.

## Як Це Зробити:
```Bash
# Рядок для роботи
str="Привіт, це мій текст для роботи"
# Виділяємо підрядок від 8 по 10 символи (починаючи з 0)
echo ${str:7:10}
```
Вихід буде такий:
```Bash
це мій т
```
## Поглиблений Аналіз:
Історичний контекст: виділення підрядків на Bash з'явилося ще в перших версіях, роблячи манипуляції з рядками простішими та ефективнішими.

Альтернативи: Крім синтаксису `${str:position:length}`, можна використовувати утиліту `cut`.

Реалізація: Bash виконує екстракцію підрядків, просто перескакуючи до вказаного символу в рядку і продовжуючи читати відтіля задану кількість символів.

## Дивіться Також:
1. [Руководство по Bash от GNU](https://www.gnu.org/software/bash/manual/bash.html)
2. [Туториал по Bash від Ryans Tutorials](https://ryanstutorials.net/bash-scripting-tutorial/bash-string-manipulation.php)
3. [Подробиці про виділення підрядків в Bash на StackOverflow](https://stackoverflow.com/questions/4277665/bash-how-do-i-extract-the-substrings-of-a-string)