---
title:                "Обчислення дати у майбутньому чи минулому"
aliases:
- /uk/bash/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:28:31.922931-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому чи минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Розрахунки дат у майбутньому чи минулому — це визначення нових дат, додавши чи віднявши час до відомої дати. Програмісти роблять це для напланування подій, термінів завдань та архівації.

## How to: (Як це зробити:)
```Bash
# Add days to the current date
date -d "+5 days" '+%Y-%m-%d'
```

```Bash
# Subtract days from the current date
date -d "-5 days" '+%Y-%m-%d'
```

```Bash
# Use a specific date as a starting point
date -d "2023-03-15 +10 days" '+%Y-%m-%d'
```

```Bash
# Output examples
2023-03-20 # Five days added to the current date
2023-03-10 # Five days subtracted from the current date
2023-03-25 # Ten days added to March 15, 2023
```

## Deep Dive (Поглиблений Аналіз):
Праця з датами — це стандартний кусок роботи для баш. Від днів Unix, `date` був основним інструментом. Альтернативи включають `GNU date`, `datetime` модулі в Python, або використання `dateutils` в Linux.

Налаштування баш скриптів для роботи з датами важливе для автоматизації та логування. Більшість систем підтримують цю команду без додаткових інсталяцій.

## See Also (Дивись також):
- GNU Coreutils Manual: https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation
- `dateutils` documentation: http://www.fresse.org/dateutils/
- Bash script examples for date manipulation: https://mywiki.wooledge.org/BashFAQ/081
