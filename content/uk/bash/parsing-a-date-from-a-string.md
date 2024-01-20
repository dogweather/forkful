---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:35:03.583128-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Парсинг дати з рядка — це процес витягу інформації про дату з текстової строки. Програмісти роблять це, щоб обробляти і зберігати дати в структурному форматі, зручному для використання у програмах.

## Як це робити:
```Bash
# Використовуємо date з +FORMAT для форматування введеної дати
date_input="2023-03-17 14:35:00"
parsed_date=$(date -d "$date_input" '+%A, %d %B %Y')
echo $parsed_date
# Вивід: П'ятниця, 17 березень 2023

# Використовуємо awk для витягу частин дати
echo $date_input | awk -F[-' '] '{print "День: " $3 ", Місяць: " $2 ", Рік: " $1}'
# Вивід: День: 17, Місяць: 03, Рік: 2023
```

## Поглиблено:
Парсинг дат вивчається давно і є дуже важливим, адже дати є скрізь: від журналів до баз даних. Баш, історично, не найкращий інструмент для цього через обмеження стандартних утиліт, але `date` дозволяє перетворювати строки в дати і навпаки. Альтернативою може бути використання мов програмування з потужнішими бібліотеками для дат, як Python чи JavaScript. Тільки пам'ятайте про локалізацію: у різних країнах формати дат можуть відрізнятися.

## Також дивіться:
- [GNU Coreutils - Date Documentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html) для детальнішого розуміння команди `date`.
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/) щоб поліпшити розуміння Bash.
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/index.html) для комплексного поглиблення в скриптуванні на Bash.
- `man awk` в терміналі для отримання інформації про `awk` команду.