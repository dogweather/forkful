---
title:                "Перетворення рядка у нижній регістр"
date:                  2024-01-20T17:37:55.249155-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що & Чому?
Перетворення рядків у нижній регістр - це зміна всіх великих літер на маленькі. Програмісти це роблять для уніфікації даних, пошуку та порівняння рядків без звернення уваги на регістр.

## Як?
```Bash
# Використовуємо 'tr' для перетворення рядків у нижній регістр
echo "HELLO World!" | tr '[:upper:]' '[:lower:]'
# Вивід: hello world!

# Ще один спосіб - команда 'awk'
echo "HELLO World!" | awk '{print tolower($0)}'
# Вивід: hello world!

# Використання 'sed'
echo "HELLO World!" | sed 's/.*/\L&/'
# Вивід: hello world!

# Перетворення за допомогою Bash вбудованих функцій
my_string="HELLO World!"
echo "${my_string,,}"
# Вивід: hello world!
```

## Поглиблено
У Bash було декілька способів робити перетворення рядка у нижній регістр. Історично, команди як `tr`, `awk`, і `sed` використовувалися, оскільки ранні версії Bash не мали вбудованої підтримки цього. 

`tr` - одна з найдавніших утиліт для роботи з текстом, вона змінює або видаляє символи. `awk` є скриптовою мовою для обробки даних, і вона може викликати функції обробки рядків. `sed` - потоковий редактор для фільтрації та перетворення тексту.

З Bash 4.0, введено нову функціональність, `${variable,,}` та `${variable^^}` для вбудованої підтримки перетворення регістру рядка.

Кожен інструмент має свої особливості. `tr` не працює з змінними напряму і не приймає файл як аргумент. `awk` і `sed` потужніші, але вимагають більш складної синтаксис та розуміння. Вбудовані функції Bash найкращі для простих скриптів, бо вони швидкі і не вимагають зовнішніх програм.

## Також подивіться
- [Bash Reference Manual](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion)
- [GNU 'tr' manual](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html#tr-invocation)
- [AWK Language Programming](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Sed - An Introduction and Tutorial](https://www.gnu.org/software/sed/manual/sed.html)
