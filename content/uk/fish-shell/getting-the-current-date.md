---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:14:38.386126-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Отримання поточної дати — це просто дізнатися, який сьогодні день. Програмісти це роблять, щоб логувати події, виміряти час виконання завдань чи забезпечити динамічні функції, як-от планувальники.

## How to: (Як це зробити:)
```Fish Shell
# Вивід поточної дати в стандартному форматі
set current_date (date)
echo $current_date
# Очікуваний результат:
# Tue Mar 14 15:20:45 EET 2023

# Форматувати дату для виводу тільки року, місяця і дня
set formatted_date (date "+%Y-%m-%d")
echo $formatted_date
# Очікуваний результат:
# 2023-03-14
```

## Deep Dive (Поглиблений Аналіз)
Отримання дати у Fish Shell використовує команду `date`, яка існує у багатьох Unix-подібних системах з давніх часів. Сучасні шелі, як Bash чи Zsh, також підтримують цю команду. Fish Shell, що з'явився пізніше, унаслідував цю функціональність. Використання `date` дозволяє отримати дату шляхом форматування виводу в будь-якому потрібному форматі, використовуючи `strftime` коди форматування. Ще одним варіантом може бути використання команди `cal`, що виводить календар на місяць, якщо вам потрібен огляд.

## See Also (Див. також):
- Офіційний сайт Fish Shell: https://fishshell.com/
- Документація команди `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Туторіал з форматування дати і часу у Fish Shell: https://fishshell.com/docs/current/cmds/date.html
- Інформація про `strftime` коди форматування: https://www.man7.org/linux/man-pages/man3/strftime.3.html
