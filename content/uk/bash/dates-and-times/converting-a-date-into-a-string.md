---
title:                "Перетворення дати в рядок"
aliases:
- /uk/bash/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:11.132628-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Перетворюємо дату в рядок, щоб зручно зберігати або відображати її. Це важливо для логування, користувацького інтерфейсу, чи форматування дат під конкретні вимоги.

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
