---
date: 2024-01-20 17:36:11.132628-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) ."
lastmod: '2024-04-05T21:53:49.733353-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

## How to: (Як це зробити:)
```Bash
# Отримуємо поточну дату у вигляді рядка
current_date=$(date '+%Y-%m-%d')
echo $current_date
# Output: 2023-04-12

# Конвертуємо конкретну дату
specified_date=$(date -d '2023-12-01' '+%A, %d %B %Y')
echo $specified_date
# Output: Friday, 01 December 2023

# Форматуємо часову мітку у читабельний формат
timestamp=$(date -d '@2147483647' '+%Y-%m-%d %H:%M:%S')
echo $timestamp
# Output: 2038-01-19 03:14:07
```

## Deep Dive (Поглиблений Розбір):
Дата-в-рядок – це типова завдання в програмуванні, що з'явилася з потреби в єдиному форматі зберігання та обміну датами. POSIX визначив утиліту `date` для роботи з датами та часом в UNIX-подібних системах, дозволяючи конвертувати часові мітки (timestamps) у зручні для людини формати. Альтернативами є бібліотеки мов програмування як, наприклад, `DateTime` у Perl або `datetime` у Python. Додатково, в Bash ви можете користуватися утилітою `awk`, якою можна парсити і форматувати дату, використовуючи вбудовані функції.

## See Also (Дивіться також):
- Документація GNU Coreutils `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Туторіал по `awk`: https://www.gnu.org/software/gawk/manual/gawk.html
- Більше про часові мітки в Unix: https://en.wikipedia.org/wiki/Unix_time
