---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Javascript: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і чому?

Перетворення рядка на нижній регістр - це процес заміни всіх великих літер у рядку на маленькі. Програмісти це роблять, щоб нормалізувати дані, уникнувши розрізнення регістру.

## Як це зробити:

Мова Bash має вбудовану функціональність для цього. Використовуйте нижченаведений код:

```Bash
str="HELLO WORLD"
echo "${str,,}"
```

Вихідний код:

```Bash
hello world
```

## Поглиблений підхід

Сучасні версії Bash (версії 4.0 і вище) включають вбудовану опцію для переведення символів у нижній регістр. В історичному контексті, в інших мовах можуть бути альтернативні методи, наприклад використання `tr 'A-Z' 'a-z'`. Однак, в вбудованому методу менше потребує коду та він більш ефективний.

## Див. також

Для більш детальної інформації, ви можете звернутися до наступних джерел:

- [GNU Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Lowercase in Bash](https://www.cyberciti.biz/faq/linux-unix-shell-programming-converting-lowercase-uppercase/)
- [StackOverflow: convert string to lower case](https://stackoverflow.com/questions/2264428/how-to-convert-a-string-to-lower-case-in-bash)