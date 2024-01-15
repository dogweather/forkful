---
title:                "Видалення символів, що відповідають шаблону"
html_title:           "Bash: Видалення символів, що відповідають шаблону"
simple_title:         "Видалення символів, що відповідають шаблону"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Чому

Є багато можливих причин для видалення символів, що відповідають певному шаблону. Наприклад, це може бути необхідно для очищення тексту перед подальшою обробкою або для заміни помилкових даних.

## Як це зробити

```Bash
# Видалення всіх цифр зі строки
echo "abcd123efg" | sed 's/[0-9]//g'
# Результат: abcdefg
```

```Bash
# Видалення всіх не цифр зі строки
echo "abcd123efg" | sed 's/[^0-9]//g'
# Результат: 123
```

```Bash
# Видалення повторюваних символів
echo "aaabbbccc" | sed 's/\(.\)\1\+//g'
# Результат: abc
```

## Глибокий погляд

Утиліта sed є дуже потужним інструментом для заміни тексту. Команда `s/pattern/replace/` замінює перше входження шаблону на підстроку, а `s/pattern/replace/g` замінює всі входження. Можна використовувати регулярні вирази для визначення шаблону, що дає більшу гнучкість при заміні символів.

## Дивіться також

- [Регулярні вирази в Bash](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-bash)
- [Офіційна документація sed](https://www.gnu.org/software/sed/manual/sed.html)